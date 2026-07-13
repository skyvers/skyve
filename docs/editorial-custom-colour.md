# Editorial custom colour schemes

The free Editorial template supports client branding without a paid PrimeFaces theme or a Sass build. Colour schemes are thin CSS files that override a few `--skyve-editorial-*` variables; the rest of the layout (divider, focus ring, menu highlights, buttons, table actions) derives from those values automatically.

This is the Editorial equivalent of editing Ecuador SCSS tokens such as:

```scss
$primaryColor: #004B88 !default;
$primaryLightColor: scale-color($primaryColor, $lightness: 60%) !default;
$primaryDarkColor: scale-color($primaryColor, $lightness: -10%) !default;
$primaryDarkerColor: scale-color($primaryColor, $lightness: -20%) !default;
$primaryTextColor: #ffffff !default;
```

## Built-in colours

Set the theme colour on the UxUi in `skyve-war/src/main/java/router/UxUis.java`:

```java
private static final String THEME_COLOUR = "custom";
```

Available values: `auto` (default), `blue`, `indigo`, `emerald`, `custom`, and `dark`.

- `auto` follows the browser/OS colour scheme (`blue` in light mode, `dark` in dark mode).
- `dark` forces the dark palette permanently.
- `custom` is a starter corporate palette (`#004B88`) intended to be edited per project.

Example:

```java
UxUi.newPrimeFaces("phone", "editorial", "skyve", "custom");
```

## How a colour is wired

Each colour name pairs two stylesheets. PrimeFaces resolves the theme as `skyve-<colour>` via `UxUi.getPfTheme()`.

1. **PrimeFaces alias** (`theme.css`) — imports free saga (or arya for dark) so widget internals render.
2. **Layout palette** — overrides `--skyve-editorial-*` CSS variables. Loaded after `main.css` from the Editorial template.

| Kind | PrimeFaces alias location | Layout palette location |
|---|---|---|
| Built-in (`auto`, `blue`, `indigo`, `emerald`, `dark`) | `skyve-web/src/main/resources/META-INF/resources/primefaces-skyve-<colour>/` | `skyve-war/.../editorial/assets/css/themes/<colour>.css` |
| Project branding (`custom`, or your own name) | `skyve-war/src/main/webapp/WEB-INF/resources/primefaces-skyve-<colour>/` | same themes folder as above |

Put project-specific aliases in the **WAR** under `WEB-INF/resources`. That way they deploy with the application and do not require rebuilding `skyve-web`. A missing alias produces:

`Error loading CSS, cannot find "theme.css" resource of "primefaces-skyve-<colour>" library`

## Create a new colour scheme

1. **Copy the starter palette**:

   ```bash
   cp skyve-war/src/main/webapp/WEB-INF/resources/skyve/editorial/assets/css/themes/custom.css \
      skyve-war/src/main/webapp/WEB-INF/resources/skyve/editorial/assets/css/themes/mybrand.css
   ```

2. **Edit the brand knobs** in `mybrand.css`:

   ```css
   :root {
       /* Primary brand colour as R, G, B (used for rgba() derived tokens) */
       --skyve-editorial-accent-rgb: 0, 75, 136;
       /* Darker hover / pressed accent (≈ Sass primaryDark) */
       --skyve-editorial-accent-hover: #00447A;
       /* Darker chrome such as the top bar (≈ Sass primaryDarker) */
       --skyve-editorial-topbar-bg: #003C6D;
   }
   ```

   | Ecuador SCSS | Editorial CSS variable |
   |---|---|
   | `$primaryColor` | `--skyve-editorial-accent-rgb` (and derived `--skyve-editorial-accent`) |
   | `$primaryDarkColor` | `--skyve-editorial-accent-hover` |
   | `$primaryDarkerColor` | `--skyve-editorial-topbar-bg` |
   | `$primaryLightColor` | usually leave derived (`accent-soft`, `focus-ring`); override only if needed |
   | `$primaryTextColor` | white on accent buttons by default; no variable required for typical branding |

3. **Add the PrimeFaces alias in the WAR** (light schemes all import saga):

   ```bash
   mkdir -p skyve-war/src/main/webapp/WEB-INF/resources/primefaces-skyve-mybrand
   ```

   Create `theme.css` in that folder:

   ```css
   @import url("#{resource['primefaces-saga:theme.css']}");
   ```

4. **Select it** in `UxUis.java`:

   ```java
   private static final String THEME_COLOUR = "mybrand";
   ```

5. Redeploy the WAR (or republish in the IDE) and hard-refresh the browser.

### Or edit `custom` in place

For a single-client project it is often enough to edit:

- `skyve-war/.../themes/custom.css`
- `skyve-war/.../WEB-INF/resources/primefaces-skyve-custom/theme.css` (only if you change the PF base theme)

and keep `THEME_COLOUR = "custom"`.

## Optional overrides

Most branding only needs the three knobs above. If a surface still looks wrong, override additional tokens from `main.css` in the same `:root` block, for example:

```css
:root {
	--skyve-editorial-accent-rgb: 0, 75, 136;
	--skyve-editorial-accent-hover: #00447A;
	--skyve-editorial-topbar-bg: #003C6D;
	--skyve-editorial-page-bg: #f5f7fa;
	--skyve-editorial-sidebar-bg: #ffffff;
}
```

Inspect `:root` at the top of `editorial/assets/css/main.css` for the full token list.

## Limits vs paid Ecuador themes

Editorial colour schemes recolour layout chrome and the PrimeFaces controls that Editorial restyles through CSS variables (buttons, panels, menus, and similar). Stock saga rules that hard-code their own blues are not regenerated the way Ecuador’s SCSS compile pipeline does. For deep widget recolouring without CSS overrides, use a paid PrimeFaces theme (see `primefaces-themes.txt`).

## Dark logos

If a customer resource named `<logoName>-dark.<extension>` exists beside the configured logo (for example `mylogo-dark.png` for `mylogo.png`), it is used automatically when the dark palette is active (`dark`, or `auto` when the browser prefers dark).
