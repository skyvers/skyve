/**
 * Bound input widget implementations that accept user data entry.
 *
 * <p>All widgets in this package extend {@code InputWidget} (or
 * {@code FocusableInputWidget} / {@code ChangeableInputWidget}) and are bound to a
 * document attribute. The rendered control type is chosen by the renderer; the metadata
 * here records the declared widget type and its configuration.
 *
 * <p>Widget types:
 * <ul>
 *   <li>{@code TextField} — single-line text entry with optional format mask.
 *   <li>{@code TextArea} — multi-line plain-text entry.
 *   <li>{@code RichText} — rich-text / HTML editor.
 *   <li>{@code Password} — masked password entry.
 *   <li>{@code Combo} — drop-down selection from a domain or query.
 *   <li>{@code Radio} — radio-button group selection.
 *   <li>{@code CheckBox} — boolean check box.
 *   <li>{@code CheckMembership} / {@code ListMembership} / {@code MembershipWidget} —
 *       multi-select membership widgets.
 *   <li>{@code LookupDescription} / {@code LookupDescriptionColumn} — association
 *       lookup with auto-complete.
 *   <li>{@code Spinner} — numeric spinner with optional step.
 *   <li>{@code Slider} — numeric range slider.
 *   <li>{@code ColourPicker} — colour selection widget.
 *   <li>{@code ContentImage} / {@code ContentLink} / {@code ContentSignature} —
 *       content-item upload widgets.
 *   <li>{@code Geometry} / {@code GeometryMap} / {@code GeometryInputType} —
 *       spatial geometry input widgets.
 *   <li>{@code HTML} — renders a raw HTML fragment without editing.
 *   <li>{@code DefaultWidget} — a placeholder that delegates the actual widget choice
 *       to the renderer based on the attribute type.
 *   <li>{@code Comparison} — displays two values for side-by-side comparison.
 *   <li>{@code CompleteType} — enumeration of auto-complete strategies.
 *   <li>{@code KeyboardType} — hint for mobile-keyboard type selection.
 * </ul>
 *
 * @see org.skyve.impl.metadata.view.widget.bound
 * @see org.skyve.metadata.view.widget.bound.input
 */
package org.skyve.impl.metadata.view.widget.bound.input;
