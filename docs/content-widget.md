# Managed Content Widget

Skyve 10 introduces a single view widget for managed content fields:
`<content/>`. It replaces the old view-level `contentLink` and `contentImage`
widgets and is the only active non-BizPort content upload surface.

The widget can be bound to document attributes with type `content` or `image`.
The `AttributeType.image` document field type remains supported; there is no
`AttributeType.video` document field type in this release. Video files are stored
as `content` attributes and rendered with `display="video"` or
`display="auto"`.

## Display Modes

`display` controls how stored content is presented.

| Value | Behaviour |
|---|---|
| `auto` | Default. The server classifies the stored content as `image`, `video`, or `link`, and the renderer chooses the matching presentation. |
| `link` | Renders a normal managed-content link/download presentation. |
| `image` | Renders an image preview and image actions. Valid for `content` and `image` attributes. |
| `video` | Renders a browser-native video player. Valid for `content` attributes only. |

Auto mode uses a companion media-kind value in the SmartClient edit/list JSON
payload. That payload key follows existing Skyve binding sanitisation with a
leading underscore: `media` becomes `_media`, and `attachment.media` becomes
`_attachment_media`. PrimeFaces uses its own sanitised companion identifier for
hidden fields and upload callbacks, formed as the sanitised binding plus
`_media` (for example, `attachment.media` becomes `attachment_media`). The
companion value is only the media kind (`image`, `video`, or `link`). Blank
content ids omit or clear the companion value so clients do not keep stale
media-kind state after clear.

Server-side classification uses stored MIME/content type first, after removing
MIME parameters and matching case-insensitively. Unknown or missing types fall
back to the stored file-name extension. Common image extensions such as `jpg`,
`jpeg`, `png`, `gif`, `webp`, `bmp`, and `svg` classify as image. Common video
extensions such as `mp4`, `webm`, `mov`, `m4v`, `ogv`, and `avi` classify as
video. Everything else renders as a link. SVG previews use the protected content
serving path; Skyve does not inline untrusted SVG markup.

## Capture Modes

`capture` controls which upload affordance is shown in `upload.xhtml`.

| Value | Behaviour |
|---|---|
| `none` | Default. Shows the normal choose/upload control. |
| `camera` | Shows the image/camera capture affordance only. Invalid for `display="video"`. |
| `video` | Shows the video capture affordance only. Invalid for `display="image"`. |
| `all` | Shows every valid affordance for the display mode. |

For `display="link"` and `display="auto"`, `capture="all"` allows generic file
upload, camera/photo capture, and video capture. For `display="image"`, it
allows generic image upload plus camera/photo capture. For `display="video"`, it
allows generic video upload plus video capture.

Browser support for the HTML capture hint is treated as progressive enhancement.
Desktop browsers often fall back to file pickers. Skyve still renders the
metadata-requested affordance and lets the browser choose the best available
input path.

Audio capture and audio-specific upload/rendering are deliberately out of scope:
`capture="audio"` is rejected by schema/metadata validation, and there is no
audio upload category or audio renderer branch.

## Dimensions

Image dimensions use the same relative/fixed sizing metadata as other bound
widgets, including `pixelWidth`, `pixelHeight`, responsive widths, minimums, and
maximums. Video rendering uses browser-native controls with stable defaults:

| Context | Default video size |
|---|---|
| Form/edit view | `320 x 180` |
| Grid/list cell | `160 x 90` |

Override dimensions in metadata when a view needs a different size.

## Upload Page

`upload.xhtml` is the unified non-BizPort upload page for:

- bound managed-content/image uploads from `<content/>`
- action `<upload>` file uploads

The old active pages `contentUpload.xhtml`, `imageUpload.xhtml`, and
`fileUpload.xhtml` have been removed. BizPort import remains separate.
`imageMarkup.xhtml` and `ContentSignature` also remain separate and image-only.

Upload routes provide enough state for the page to validate the request without
guessing: upload kind (`boundContent` or `action`), conversation context,
binding/content binding or action name, display mode, capture mode, optional
auto-mode companion field, and callback target. The page derives its internal
mode from validated metadata and action configuration, not from arbitrary client
input.

After a bound upload, Skyve updates the bound content id and, for auto-mode
widgets, the companion media-kind field. Clearing content clears both values.
Action uploads preserve the previous action semantics: uploaded bytes are passed
to the upload action path and no bound content id is required.

The upload page follows the existing XHTML upload security model. It may render
inside an iframe so it can show a no-access state, but upload processing still
requires a Skyve user session and the normal content/document or action access
checks.

## Upload Configuration

Upload limits are selected by the chosen affordance:

| Affordance | Configuration bucket |
|---|---|
| Normal action upload | `uploads.file` |
| Generic bound content upload | `uploads.content` |
| Image file/camera/crop upload | `uploads.image` |
| Video file/capture upload | `uploads.video` |
| BizPort import | `uploads.bizport` |

The default video whitelist is `^.+\.(MP4|WEBM|MOV|M4V|OGV|AVI)$`. The default
video maximum size is `100` MB, chosen as a deterministic limit large enough for
about one minute of typical mobile video and larger than the default image/file
limit.

Configure the video bucket in the application JSON when an application needs a
different policy:

```json
"uploads": {
	"video": {
		"whitelistRegex": "^.+\\.(MP4|WEBM|MOV|M4V)$",
		"maximumSizeMB": 150
	}
}
```

## Examples

Link-like attachment:

```xml
<content binding="attachment" display="link" />
```

Image/photo, including camera capture:

```xml
<content binding="photo" display="image" capture="camera" pixelWidth="240" pixelHeight="180" />
```

Video:

```xml
<content binding="clip" display="video" capture="video" />
```

Auto mixed attachment with all supported upload choices:

```xml
<content binding="media" display="auto" capture="all" />
```

Action upload with capture:

```xml
<upload name="UploadEvidence"
		displayName="Upload Evidence"
		className="UploadEvidence"
		capture="all" />
```

## Migration

Use `<content/>` in new metadata and migrate old view widgets as follows:

| Old view metadata | New view metadata |
|---|---|
| `<contentLink binding="file"/>` | `<content binding="file" display="link"/>` |
| `<contentImage binding="photo"/>` | `<content binding="photo" display="image"/>` |
| mixed content field | `<content binding="attachment" display="auto"/>` |

The removed names are no longer accepted as active view widgets.
