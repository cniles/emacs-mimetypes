# mimetypes.el -- an elisp library for guessing mimetypes

Uses platform-dependent configurations for guessing the MIME type of a
file extension.

## Windows

The Windows registry is queried. `HKEY_LOCAL_MACHINE\Software\Classes`
is checked for an entry for the extension.  If the `Content Type` value under the
extension's registry entry exists that is returned.

## *nix

Guessing is done with a list of known mime.types files.  See variable
`mimetypes-known-files`.  The first file found is searched for the
desired extension.  If one is not found, the remaining files are not
checked for existence or searched.

## MS-Dos / Cygwin

Determination of a mechanism, implementation and testing is still
TODO.  If needed please open a ticket.

## extra-types argument

The optional argument `extra-types` can be used to specify a list of
additional MIME types.  Each element should be a list of strings.  The
first element being the MIME type, the remaining items being the
mapped file extensions.

## User .mime.types file

If a user-provided .mime.types file is found, the value provided there
takes precendence over the platform-determined mechanism and
extra-types list.  If a MIME type isn't found for the extension in
this file the platform-determined mechanism is used.


For Windows, this file is searched for under `%USERPROFILE\.mime.types`.

For all other platforms, `$HOME/.mime.types` is searched.

## Examples

```
(mimetypes-extension-to-mime ".jpg")
-> "image/jpeg"

(mimetypes-extension-to-mime "txt")
-> "text/plain"

(mimetypes-extension-to-mime "yaml" '(("application/yaml" "yml' "yaml")))
-> "application/yaml"
```
