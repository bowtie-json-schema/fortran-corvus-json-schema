# The C library and the Fortran binding, as released (src-rs/corvus-json-schema-capi and src-fortran/corvus-json-schema
# in Corvus.JsonSchema). Their versions are these tags, which Dependabot keeps up to date.
FROM ghcr.io/corvus-dotnet/corvus-json-schema-c:0.1.4 AS corvus
FROM ghcr.io/corvus-dotnet/corvus-json-schema-fortran:0.1.0 AS binding

FROM alpine:3.24 AS build
RUN apk add --no-cache gfortran musl-dev
COPY --from=corvus /corvus-json-schema/linux-musl /opt/corvus-json-schema
COPY --from=binding /corvus-json-schema /opt/corvus-json-schema-fortran
COPY bowtie_corvus_json_schema.f90 /src/
# The binding's module and the harness, linked with the C library's static library.
RUN cd /src && gfortran -O2 -std=f2018 -Wall -Wextra -Werror -Wno-maybe-uninitialized \
      -o bowtie_corvus_json_schema /opt/corvus-json-schema-fortran/src/corvus_json_schema.f90 bowtie_corvus_json_schema.f90 \
      /opt/corvus-json-schema/lib/libcorvus_json_schema.a

FROM alpine:3.24
RUN apk add --no-cache libgfortran libgcc
COPY --from=build /src/bowtie_corvus_json_schema /usr/local/bin/
CMD ["bowtie_corvus_json_schema"]
