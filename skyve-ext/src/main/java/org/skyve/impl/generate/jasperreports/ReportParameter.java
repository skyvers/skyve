package org.skyve.impl.generate.jasperreports;

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
	public String getName() {
		return name;
	}

	/**
	 * Sets the parameter name used in Jasper parameter declarations.
	 *
	 * @param name The parameter name.
	 */
	public void setName(String name) {
		this.name = name;
	}
	
	/**
	 * Returns the Java class name used for the Jasper parameter type.
	 *
	 * @return The parameter type class name.
	 */
	public String getTypeClass() {
		return typeClass;
	}

	/**
	 * Sets the Java class name used for the Jasper parameter type.
	 *
	 * @param typeClass The parameter type class name.
	 */
	public void setTypeClass(String typeClass) {
		this.typeClass = typeClass;
	}

	/**
	 * Returns the Jasper default-value expression for this parameter.
	 *
	 * @return The default-value expression.
	 */
	public String getDefaultValueExpression() {
		return defaultValueExpression;
	}

	/**
	 * Sets the Jasper default-value expression for this parameter.
	 *
	 * @param defaultValueExpression The default-value expression.
	 */
	public void setDefaultValueExpression(String defaultValueExpression) {
		this.defaultValueExpression = defaultValueExpression;
	}

	/**
	 * Renders this parameter descriptor as a JRXML parameter fragment.
	 *
	 * @return JRXML for this parameter.
	 */
	public String getJrxml() {
		return Renderer.renderParameter(this);
	}

	/**
	 * Returns the parent design specification that owns this parameter.
	 *
	 * @return The parent design specification.
	 */
	public DesignSpecification getParent() {
		return parent;
	}
	
	/**
	 * Sets the parent design specification that owns this parameter.
	 *
	 * @param parent The parent design specification.
	 */
	public void setParent(DesignSpecification parent) {
		this.parent = parent;
	}
}
