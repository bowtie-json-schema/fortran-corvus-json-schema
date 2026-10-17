!> A Bowtie (https://github.com/bowtie-json-schema/bowtie) harness for the Fortran binding to the corvus-json-schema C
!> library.
!>
!> It speaks IHOP (one JSON request per line on standard input, one response per line on standard output):
!> - start reports the implementation and its dialects;
!> - dialect sets the dialect for schemas without $schema;
!> - run compiles the case's schema with the case's registry as the document resolver and validates each instance
!>   (for annotations output, through a verbose collector, reporting each annotation with its instance location and
!>   #... keyword location);
!> - stop exits.
!>
!> Requests are read with a small JSON scanner that keeps each value's text as Bowtie wrote it, so schemas and
!> instances reach the library unchanged (numbers included).
module ihop
  use corvus_json_schema
  implicit none
  private

  public :: span_t, value_at, member, member_string, raw, entries, entry_t, json_string, percent_encoded
  public :: unescaped_token, set_registry, registry_resolver, put

  !> A JSON value within a text: its first and last characters (last < first for none).
  type :: span_t
    integer :: first = 1
    integer :: last = 0
  end type span_t

  !> A member (its name unescaped) or an element (no name) of an object or array.
  type :: entry_t
    character(:), allocatable :: name
    type(span_t) :: value
  end type entry_t

  type :: registered_t
    character(:), allocatable :: uri
    character(:), allocatable :: json
  end type registered_t

  type(registered_t), allocatable, save :: registry(:)

contains

  subroutine skip_ws(text, pos)
    character(*), intent(in) :: text
    integer, intent(inout) :: pos
    do while (pos <= len(text))
      if (index(" " // achar(9) // achar(10) // achar(13), text(pos:pos)) == 0) return
      pos = pos + 1
    end do
  end subroutine skip_ws

  ! Moves pos past one value, returning .false. for malformed text.
  recursive function skip(text, pos) result(ok)
    character(*), intent(in) :: text
    integer, intent(inout) :: pos
    logical :: ok
    character :: c, close
    ok = .false.
    call skip_ws(text, pos)
    if (pos > len(text)) return
    c = text(pos:pos)
    if (c == '"') then
      pos = pos + 1
      do while (pos <= len(text))
        if (text(pos:pos) == "\") then
          pos = pos + 2
          cycle
        end if
        if (text(pos:pos) == '"') then
          pos = pos + 1
          ok = .true.
          return
        end if
        pos = pos + 1
      end do
      return
    end if
    if (c == "{" .or. c == "[") then
      close = merge("}", "]", c == "{")
      pos = pos + 1
      if (expect(text, pos, close)) then
        ok = .true.
        return
      end if
      do
        if (c == "{") then
          if (.not. skip(text, pos)) return
          if (.not. expect(text, pos, ":")) return
        end if
        if (.not. skip(text, pos)) return
        if (.not. expect(text, pos, ",")) exit
      end do
      ok = expect(text, pos, close)
      return
    end if
    do while (pos <= len(text))
      if (index(",]} " // achar(9) // achar(10) // achar(13), text(pos:pos)) > 0) exit
      pos = pos + 1
    end do
    ok = .true.
  end function skip

  logical function expect(text, pos, c)
    character(*), intent(in) :: text
    integer, intent(inout) :: pos
    character, intent(in) :: c
    call skip_ws(text, pos)
    expect = .false.
    if (pos > len(text)) return
    if (text(pos:pos) /= c) return
    pos = pos + 1
    expect = .true.
  end function expect

  !> The value that starts at or after pos, moving pos past it.
  function value_at(text, pos, ok) result(value)
    character(*), intent(in) :: text
    integer, intent(inout) :: pos
    logical, intent(out) :: ok
    type(span_t) :: value
    call skip_ws(text, pos)
    value%first = pos
    ok = skip(text, pos)
    value%last = pos - 1
  end function value_at

  function raw(text, value) result(written)
    character(*), intent(in) :: text
    type(span_t), intent(in) :: value
    character(:), allocatable :: written
    written = text(value%first:value%last)
  end function raw

  ! The UTF-8 bytes of a code point.
  function utf8(cp) result(bytes)
    integer, intent(in) :: cp
    character(:), allocatable :: bytes
    if (cp < 128) then
      bytes = char(cp)
    else if (cp < 2048) then
      bytes = char(ior(192, ishft(cp, -6))) // char(ior(128, iand(cp, 63)))
    else if (cp < 65536) then
      bytes = char(ior(224, ishft(cp, -12))) // char(ior(128, iand(ishft(cp, -6), 63))) // char(ior(128, iand(cp, 63)))
    else
      bytes = char(ior(240, ishft(cp, -18))) // char(ior(128, iand(ishft(cp, -12), 63))) // &
              char(ior(128, iand(ishft(cp, -6), 63))) // char(ior(128, iand(cp, 63)))
    end if
  end function utf8

  integer function hex4(text, at)
    character(*), intent(in) :: text
    integer, intent(in) :: at
    integer :: status
    read (text(at:at + 3), "(z4)", iostat=status) hex4
    if (status /= 0) hex4 = -1
  end function hex4

  !> A string literal's UTF-8 text, unescaped; ok is .false. when it is not a string.
  function unescape(text, literal, ok) result(value)
    character(*), intent(in) :: text
    type(span_t), intent(in) :: literal
    logical, intent(out) :: ok
    character(:), allocatable :: value
    integer :: p, finish, cp, low
    character :: c
    value = ""
    ok = .false.
    if (literal%last - literal%first < 1) return
    if (text(literal%first:literal%first) /= '"') return
    p = literal%first + 1
    finish = literal%last - 1
    do while (p <= finish)
      if (text(p:p) /= "\") then
        value = value // text(p:p)
        p = p + 1
        cycle
      end if
      p = p + 1
      if (p > finish) return
      c = text(p:p)
      p = p + 1
      select case (c)
      case ('"', "\", "/"); value = value // c
      case ("b"); value = value // achar(8)
      case ("f"); value = value // achar(12)
      case ("n"); value = value // achar(10)
      case ("r"); value = value // achar(13)
      case ("t"); value = value // achar(9)
      case ("u")
        if (finish - p + 1 < 4) return
        cp = hex4(text, p)
        if (cp < 0) return
        p = p + 4
        if (cp >= 55296 .and. cp < 56320 .and. finish - p + 1 >= 6) then
          if (text(p:p + 1) == "\u") then
            low = hex4(text, p + 2)
            if (low >= 56320 .and. low < 57344) then
              cp = 65536 + (cp - 55296) * 1024 + (low - 56320)
              p = p + 6
            end if
          end if
        end if
        value = value // utf8(cp)
      case default
        return
      end select
    end do
    ok = .true.
  end function unescape

  !> The members of an object (names unescaped) or the elements of an array.
  subroutine entries(text, container, items, ok)
    character(*), intent(in) :: text
    type(span_t), intent(in) :: container
    type(entry_t), allocatable, intent(out) :: items(:)
    logical, intent(out) :: ok
    type(entry_t), allocatable :: grown(:)
    type(entry_t) :: item
    type(span_t) :: key
    logical :: object, good
    integer :: pos, n
    allocate (items(0))
    ok = .false.
    pos = container%first
    call skip_ws(text, pos)
    if (pos > container%last) return
    object = text(pos:pos) == "{"
    if (.not. expect(text, pos, merge("{", "[", object))) return
    if (expect(text, pos, merge("}", "]", object))) then
      ok = .true.
      return
    end if
    n = 0
    do
      if (object) then
        key = value_at(text, pos, good)
        if (.not. good) return
        item%name = unescape(text, key, good)
        if (.not. good) return
        if (.not. expect(text, pos, ":")) return
      else
        item%name = ""
      end if
      item%value = value_at(text, pos, good)
      if (.not. good) return
      n = n + 1
      allocate (grown(n))
      grown(1:n - 1) = items
      grown(n) = item
      call move_alloc(grown, items)
      if (.not. expect(text, pos, ",")) exit
    end do
    ok = expect(text, pos, merge("}", "]", object))
  end subroutine entries

  !> An object's member whose unescaped name is name (found says whether there was one).
  function member(text, object, name, found) result(value)
    character(*), intent(in) :: text
    type(span_t), intent(in) :: object
    character(*), intent(in) :: name
    logical, intent(out) :: found
    type(span_t) :: value
    type(entry_t), allocatable :: items(:)
    logical :: ok
    integer :: i
    found = .false.
    call entries(text, object, items, ok)
    do i = 1, size(items)
      if (len(items(i)%name) == len(name)) then
        if (items(i)%name == name) then
          value = items(i)%value
          found = .true.
          return
        end if
      end if
    end do
  end function member

  !> A member's string value, unescaped (found is .false. when there is none, or it is not a string).
  function member_string(text, object, name, found) result(value)
    character(*), intent(in) :: text
    type(span_t), intent(in) :: object
    character(*), intent(in) :: name
    logical, intent(out) :: found
    character(:), allocatable :: value
    type(span_t) :: v
    value = ""
    v = member(text, object, name, found)
    if (found) value = unescape(text, v, found)
  end function member_string

  !> A JSON string literal for UTF-8 text.
  function json_string(value) result(literal)
    character(*), intent(in) :: value
    character(:), allocatable :: literal
    character(6) :: escape
    integer :: i, c
    literal = '"'
    do i = 1, len(value)
      c = ichar(value(i:i))
      if (value(i:i) == '"' .or. value(i:i) == "\") then
        literal = literal // "\" // value(i:i)
      else if (c < 32) then
        write (escape, "(a,z4.4)") "\u", c
        literal = literal // escape
      else
        literal = literal // value(i:i)
      end if
    end do
    literal = literal // '"'
  end function json_string

  !> Text percent-encoded as a URI fragment is (upper-case hex, UTF-8), keeping the characters a fragment allows.
  function percent_encoded(value) result(encoded)
    character(*), intent(in) :: value
    character(:), allocatable :: encoded
    character(*), parameter :: allowed = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" // &
                                         "-._~!$&'()*+,;=:@/?"
    character(3) :: hex
    integer :: i
    encoded = ""
    do i = 1, len(value)
      if (index(allowed, value(i:i)) > 0) then
        encoded = encoded // value(i:i)
      else
        write (hex, "(a,z2.2)") "%", ichar(value(i:i))
        encoded = encoded // hex
      end if
    end do
  end function percent_encoded

  !> A JSON pointer token unescaped: ~1 is /, ~0 is ~.
  function unescaped_token(token) result(value)
    character(*), intent(in) :: token
    character(:), allocatable :: value
    integer :: i
    value = ""
    i = 1
    do while (i <= len(token))
      if (i < len(token) .and. token(i:i) == "~") then
        if (token(i + 1:i + 1) == "1") then
          value = value // "/"
          i = i + 2
          cycle
        else if (token(i + 1:i + 1) == "0") then
          value = value // "~"
          i = i + 2
          cycle
        end if
      end if
      value = value // token(i:i)
      i = i + 1
    end do
  end function unescaped_token

  function without_fragment(uri) result(base)
    character(*), intent(in) :: uri
    character(:), allocatable :: base
    integer :: hash
    hash = index(uri, "#")
    if (hash > 0) then
      base = uri(1:hash - 1)
    else
      base = uri
    end if
  end function without_fragment

  !> The case's registry (an object from URI to schema), which registry_resolver serves.
  subroutine set_registry(text, object, present_)
    character(*), intent(in) :: text
    type(span_t), intent(in) :: object
    logical, intent(in) :: present_
    type(entry_t), allocatable :: items(:)
    logical :: ok
    integer :: i
    if (allocated(registry)) deallocate (registry)
    allocate (registry(0))
    if (.not. present_) return
    call entries(text, object, items, ok)
    deallocate (registry)
    allocate (registry(size(items)))
    do i = 1, size(items)
      registry(i)%uri = without_fragment(items(i)%name)
      registry(i)%json = raw(text, items(i)%value)
    end do
  end subroutine set_registry

  subroutine registry_resolver(uri, json, found)
    character(*), intent(in) :: uri
    character(:), allocatable, intent(out) :: json
    logical, intent(out) :: found
    character(:), allocatable :: base
    integer :: i
    found = .false.
    base = without_fragment(uri)
    do i = 1, size(registry)
      if (len(registry(i)%uri) == len(base)) then
        if (registry(i)%uri == base) then
          json = registry(i)%json
          found = .true.
          return
        end if
      end if
    end do
  end subroutine registry_resolver

  !> Appends text to a growing output.
  subroutine put(out, text)
    character(:), allocatable, intent(inout) :: out
    character(*), intent(in) :: text
    out = out // text
  end subroutine put

end module ihop

program bowtie_corvus_json_schema
  use, intrinsic :: iso_fortran_env, only: compiler_version, error_unit, input_unit, output_unit
  use corvus_json_schema
  use ihop
  implicit none

  character(*), parameter :: dialect_uris(5) = [character(44) :: &
    "https://json-schema.org/draft/2020-12/schema", "https://json-schema.org/draft/2019-09/schema", &
    "http://json-schema.org/draft-07/schema#", "http://json-schema.org/draft-06/schema#", &
    "http://json-schema.org/draft-04/schema#"]
  integer, parameter :: dialect_ids(5) = [DRAFT2020_12, DRAFT2019_09, DRAFT7, DRAFT6, DRAFT4]
  character(:), allocatable :: line, cmd, out, uri
  type(span_t) :: request, version
  logical :: eof, found, started
  integer :: dialect, i

  started = .false.
  dialect = DRAFT2020_12
  do
    call read_line(line, eof)
    if (eof .and. len(line) == 0) exit
    if (len_trim(line) == 0) cycle
    request = span_t(1, len(line))
    cmd = member_string(line, request, "cmd", found)
    if (.not. found) call fail("A request has no command")
    out = ""
    select case (cmd)
    case ("start")
      version = member(line, request, "version", found)
      if (.not. found) call fail("Unsupported IHOP version")
      if (raw(line, version) /= "1") call fail("Unsupported IHOP version")
      started = .true.
      call start(out)
    case ("dialect")
      if (.not. started) call fail("Not started")
      uri = member_string(line, request, "dialect", found)
      found = .false.
      do i = 1, size(dialect_uris)
        if (uri == trim(dialect_uris(i))) then
          dialect = dialect_ids(i)
          found = .true.
        end if
      end do
      out = merge('{"ok":true} ', '{"ok":false}', found)
      out = trim(out)
    case ("run")
      if (.not. started) call fail("Not started")
      call run(line, request, out)
    case ("stop")
      if (.not. started) call fail("Not started")
      stop
    case default
      call fail("Unknown command")
    end select
    write (output_unit, "(a)") out
    flush (output_unit)
    if (eof) exit
  end do

contains

  subroutine read_line(text, at_end)
    character(:), allocatable, intent(out) :: text
    logical, intent(out) :: at_end
    character(65536) :: chunk
    integer :: count, status
    text = ""
    at_end = .false.
    do
      read (input_unit, "(a)", advance="no", size=count, iostat=status) chunk
      text = text // chunk(1:count)
      if (is_iostat_eor(status)) return
      if (status /= 0) then
        at_end = .true.
        return
      end if
    end do
  end subroutine read_line

  subroutine fail(message)
    character(*), intent(in) :: message
    write (error_unit, "(a)") message
    error stop 1
  end subroutine fail

  function file_text(path) result(text)
    character(*), intent(in) :: path
    character(:), allocatable :: text
    character(256) :: first
    integer :: unit, status
    text = ""
    open (newunit=unit, file=path, action="read", status="old", iostat=status)
    if (status /= 0) return
    read (unit, "(a)", iostat=status) first
    close (unit)
    if (status == 0) text = trim(first)
  end function file_text

  subroutine start(response)
    character(:), allocatable, intent(inout) :: response
    character(:), allocatable :: os, os_version
    os = file_text("/proc/sys/kernel/ostype")
    os_version = file_text("/proc/sys/kernel/osrelease")
    call put(response, '{"version":1,"implementation":{"language":"fortran","name":"corvus-json-schema","version":')
    call put(response, json_string(binding_version))
    call put(response, ',"homepage":"https://github.com/corvus-dotnet/Corvus.JsonSchema"' // &
             ',"documentation":"https://github.com/corvus-dotnet/corvus-json-schema-fortran"' // &
             ',"issues":"https://github.com/corvus-dotnet/Corvus.JsonSchema/issues"' // &
             ',"source":"https://github.com/corvus-dotnet/Corvus.JsonSchema","dialects":[')
    do i = 1, size(dialect_uris)
      if (i > 1) call put(response, ",")
      call put(response, json_string(trim(dialect_uris(i))))
    end do
    call put(response, "]")
    if (len(os) > 0) call put(response, ',"os":' // json_string(os))
    if (len(os_version) > 0) call put(response, ',"os_version":' // json_string(os_version))
    call put(response, ',"language_version":' // json_string("Fortran 2018 (" // compiler_version() // &
             "; C library " // library_version() // ")") // "}}")
  end subroutine start

  subroutine put_error(response, message)
    character(:), allocatable, intent(inout) :: response
    character(*), intent(in) :: message
    call put(response, '"errored":true,"context":{"message":' // json_string(message) // "}")
  end subroutine put_error

  ! The annotations a verbose collector grouped (instance location, then keyword as a JSON pointer token, then schema
  ! location as a #... fragment; the same in every Corvus implementation) as Bowtie lists them: each with its keyword
  ! unescaped, and its keyword location the schema location's fragment followed by / and the keyword token,
  ! percent-encoded as the fragment is.
  subroutine put_annotations(response, collector)
    character(:), allocatable, intent(inout) :: response
    type(collector_t), intent(inout) :: collector
    character(:), allocatable :: grouped
    type(entry_t), allocatable :: by_instance(:), by_keyword(:), by_location(:)
    logical :: ok, first
    integer :: a, k, l, stat
    call put(response, "[")
    grouped = collector%annotations_json(stat=stat)
    first = .true.
    if (stat == CJS_OK) then
      call entries(grouped, span_t(1, len(grouped)), by_instance, ok)
      do a = 1, size(by_instance)
        call entries(grouped, by_instance(a)%value, by_keyword, ok)
        do k = 1, size(by_keyword)
          call entries(grouped, by_keyword(k)%value, by_location, ok)
          do l = 1, size(by_location)
            if (.not. first) call put(response, ",")
            first = .false.
            call put(response, '{"keyword":' // json_string(unescaped_token(by_keyword(k)%name)) // &
                     ',"instanceLocation":' // json_string(by_instance(a)%name) // &
                     ',"keywordLocation":' // json_string(by_location(l)%name // "/" // &
                                                          percent_encoded(by_keyword(k)%name)) // &
                     ',"annotation":' // raw(grouped, by_location(l)%value) // "}")
          end do
        end do
      end do
    end if
    call put(response, "]")
  end subroutine put_annotations

  subroutine run(text, request_span, response)
    character(*), intent(in) :: text
    type(span_t), intent(in) :: request_span
    character(:), allocatable, intent(inout) :: response
    type(span_t) :: seq, test_case, schema, registry_object, tests_span, instance
    type(entry_t), allocatable :: tests(:)
    type(options_t) :: options
    type(validator_t) :: validator
    type(collector_t) :: collector
    character(:), allocatable :: output, errmsg
    logical :: found, has_registry, ok, annotations, valid
    integer :: stat, t

    seq = member(text, request_span, "seq", found)
    test_case = member(text, request_span, "case", found)
    schema = member(text, test_case, "schema", found)
    registry_object = member(text, test_case, "registry", has_registry)
    call set_registry(text, registry_object, has_registry)
    output = member_string(text, request_span, "output", found)
    annotations = found .and. output == "annotations"
    call put(response, '{"seq":' // raw(text, seq) // ",")
    call options%set_default_dialect(dialect)
    call options%set_resolver(registry_resolver)
    call validator%compile(raw(text, schema), options, stat=stat, errmsg=errmsg)
    if (stat /= CJS_OK) then
      call put_error(response, errmsg)
      call put(response, "}")
      return
    end if
    call put(response, '"results":[')
    tests_span = member(text, test_case, "tests", found)
    call entries(text, tests_span, tests, ok)
    if (annotations) call collector%init(VERBOSE)
    do t = 1, size(tests)
      if (t > 1) call put(response, ",")
      call put(response, "{")
      instance = member(text, tests(t)%value, "instance", found)
      if (.not. found) then
        call put_error(response, "the test has no instance")
      else if (.not. annotations) then
        valid = validator%is_valid(raw(text, instance), stat=stat, errmsg=errmsg)
        if (stat == CJS_OK) then
          call put(response, merge('"valid":true ', '"valid":false', valid))
          response = trim(response)
        else
          call put_error(response, errmsg)
        end if
      else
        valid = validator%evaluate(raw(text, instance), collector, stat=stat, errmsg=errmsg)
        if (stat == CJS_OK) then
          call put(response, trim(merge('"valid":true ', '"valid":false', valid)) // ',"annotations":')
          call put_annotations(response, collector)
        else
          call put_error(response, errmsg)
        end if
      end if
      call put(response, "}")
    end do
    call put(response, "]}")
  end subroutine run

end program bowtie_corvus_json_schema
