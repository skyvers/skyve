/**
 * Binding engine implementation: property traversal, expression evaluation, and conversion.
 *
 * <p>{@link org.skyve.impl.bind.BindUtil} is the main workhorse, implementing the
 * dot-notation binding operations exposed publicly via {@link org.skyve.util.Binder}.
 * EL expression evaluators ({@link org.skyve.impl.bind.ELExpressionEvaluator},
 * {@link org.skyve.impl.bind.BindingExpressionEvaluator}, etc.) translate specialised
 * expression prefixes ({@code {el:...}}, {@code {i18n:...}}, etc.) into resolved values.
 *
 * <p>Internal use only — the public API is {@link org.skyve.util.Binder}.
 */
package org.skyve.impl.bind;
