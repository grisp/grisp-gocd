Build Scripts for Grisp GOCD Pipelines
======================================

`install-env.sh`
------------------

Make sure `asdf` is installed and the required tool versions.
`install-env.sh ERLANG_VERSION REBAR_VERSION`


`grisp-project.sh`
--------------------

`grisp-project.sh REBAR3_GRISP_VERSION TOOLCHAIN_REVISION LINK_GRISP BUILD`

- `REBAR3_GRISP_VERSION` can be `hex` or git hash
- `TOOLCHAIN_REVISION`: `HEAD` or git hash
- `LINK_GRISP`: `true` or `false`. Link `grisp/` to `ciproject/_checkouts`
- `BUILD`: `true` or `false`

If `BUILD` is set to `true` the OTP package will be inside `$BUILDDIR/ciproject/otp/*/package/*.tar.gz`.

`deploy-toolchain.sh`
------------------

Expects toolchain to reside in `grisp-software/rtems-install/`.

