/**
 * Comparison view model SPI for side-by-side bean diff widgets.
 *
 * <p>{@link org.skyve.metadata.view.model.comparison.ComparisonModel} is the primary SPI:
 * subclass it and implement {@code getComparisonComposite(C)} to build the property
 * tree that the Skyve comparison widget renders. Each node in the tree is either a
 * {@link org.skyve.metadata.view.model.comparison.ComparisonComposite} (a section with
 * nested properties) or a {@link org.skyve.metadata.view.model.comparison.ComparisonProperty}
 * (a single field with current and other values).
 *
 * <p>{@link org.skyve.metadata.view.model.comparison.DefaultBindingComparisonModel} provides
 * a reflection-driven implementation that compares all bound attributes automatically.
 */
package org.skyve.metadata.view.model.comparison;
