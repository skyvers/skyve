package org.skyve.metadata.controller;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.report.ReportFormat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Defines deployment-level customization hooks for default UI behavior and
 * framework extension registration.
 *
 * <p>An implementation is supplied via
 * {@code factories.customisationsClass} configuration and is consulted during
 * metadata/view generation and expression/formatter bootstrap.
 *
 * <p>Threading: implementations should be thread-safe because methods may be
 * called concurrently across requests.
 */
public interface Customisations {
	/**
	 * Determine the default text alignment to use within textual widgets given an attribute type.
	 */
	@Nonnull HorizontalAlignment determineDefaultWidgetTextAlignment(@Nonnull String uxui, @Nullable AttributeType attributeType);

	/**
	 * Determine the default text alignment to use in columns given an attribute type.
	 */
	@Nonnull HorizontalAlignment determineDefaultColumnTextAlignment(@Nonnull String uxui, @Nullable AttributeType attributeType);

	/**
	 * Determine the default column width in pixels given an attribute type.
	 * <code>null</code> may be returned if there is no default.
	 */
	@Nullable Integer determineDefaultColumnWidth(@Nonnull String uxui, @Nullable AttributeType attributeType);

	/**
	 * Register custom ExpressionEvaluators for use in this Skyve deployment using ExpressionEvaluator.register().
	 */
	void registerCustomExpressions();
	
	/**
	 * Register custom Formatters for use in this Skyve deployment using Formatters.register().
	 */
	void registerCustomFormatters();

	/**
	 * Determine the list grid export formats allowed.
	 * @return	The array of allowed formats.
	 */
	@Nonnull ReportFormat[] listGridExportFormats();
	
// TODO add the view generator bit in	
//	@Nonnull ViewGenerator viewGenerator();
}
