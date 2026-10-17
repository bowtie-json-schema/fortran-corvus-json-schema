# fortran-corvus-json-schema

A [Bowtie](https://github.com/bowtie-json-schema/bowtie) test harness for the Fortran binding to the corvus-json-schema
C library, part of [Corvus.JsonSchema](https://github.com/corvus-dotnet/Corvus.JsonSchema): the module
`corvus_json_schema`, which binds the C library through `ISO_C_BINDING`
([corvus-json-schema-fortran](https://github.com/corvus-dotnet/corvus-json-schema-fortran)).

Its image is published to `ghcr.io/bowtie-json-schema/fortran-corvus-json-schema` and run via
`bowtie run -i fortran-corvus-json-schema`.

The image builds the harness (`bowtie_corvus_json_schema.f90`) with the binding's module, from the released source
in `ghcr.io/corvus-dotnet/corvus-json-schema-fortran`, and links the C library's static library, from
`ghcr.io/corvus-dotnet/corvus-json-schema-c`. Dependabot keeps both image tags in the Dockerfile up to date.
