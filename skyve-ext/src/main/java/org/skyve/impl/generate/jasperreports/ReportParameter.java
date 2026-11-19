package org.skyve.impl.generate.jasperreports;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Descriptor for a single JasperReports parameter, used when generating a
 * {@code .jrxml} programmatically from Skyve metadata.
 */
public class ReportParameter {
	/**
	 * Name
	 **/
	private String name;

	/**
	 * Class
	 **/
	private String typeClass;

	/**
	 * Default Value Expression
	 **/
	private String defaultValueExpression;
	
	private DesignSpecification parent;
	
	/**
	 * Returns the parameter name used in Jasper parameter declarations.
	 *
	 * @return The parameter name.
	 */
	public @Nullable String getName() {
		return name;
	}

	/**
	 * Sets the parameter name used in Jasper parameter declarations.
	 *
	 * @param name The parameter name.
	 */
	public void setName(@Nullable String name) {
		this.name = name;
	}
	
	/**
	 * Returns the Java class name used for the Jasper parameter type.
	 *
	 * @return The parameter type class name.
	 */
	public @Nullable String getTypeClass() {
		return typeClass;
	}

	/**
	 * Sets the Java class name used for the Jasper parameter type.
	 *
	 * @param typeClass The parameter type class name.
	 */
	public void setTypeClass(@Nullable String typeClass) {
		this.typeClass = typeClass;
	}

	/**
	 * Returns the Jasper default-value expression for this parameter.
	 *
	 * @return The default-value expression.
	 */
	public @Nullable String getDefaultValueExpression() {
		return defaultValueExpression;
	}

	/**
	 * Sets the Jasper default-value expression for this parameter.
	 *
	 * @param defaultValueExpression The default-value expression.
	 */
	public void setDefaultValueExpression(@Nullable String defaultValueExpression) {
		this.defaultValueExpression = defaultValueExpression;
	}

	/**
	 * Renders this parameter descriptor as a JRXML parameter fragment.
	 *
	 * @return JRXML for this parameter.
	 */
	public @Nonnull String getJrxml() {
		return Renderer.renderParameter(this);
	}

	/**
	 * Returns the parent design specification that owns this parameter.
	 *
	 * @return The parent design specification.
	 */
	public @Nullable DesignSpecification getParent() {
		return parent;
	}
	
	/**
	 * Sets the parent design specification that owns this parameter.
	 *
	 * @param parent The parent design specification.
	 */
	public void setParent(@Nullable DesignSpecification parent) {
		this.parent = parent;
	}
}
