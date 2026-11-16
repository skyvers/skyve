package org.skyve.impl.generate.jasperreports;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Descriptor for a JasperReports variable (aggregate expression) used during
 * programmatic {@code .jrxml} generation.
 */
public class ReportVariable {
	/**
	 * Name
	 **/
	private String name;

	/**
	 * Class
	 **/
	private String typeClass;

	/**
	 * Value
	 **/
	private String value;

	private DesignSpecification parent;

	/**
	 * Returns the Jasper variable name.
	 *
	 * @return The variable name.
	 */
	public @Nullable String getName() {
		return name;
	}

	/**
	 * Sets the Jasper variable name.
	 *
	 * @param name The variable name.
	 */
	public void setName(@Nullable String name) {
		this.name = name;
	}

	/**
	 * Returns the Java class name used for this Jasper variable type.
	 *
	 * @return The variable type class name.
	 */
	public @Nullable String getTypeClass() {
		return typeClass;
	}

	/**
	 * Sets the Java class name used for this Jasper variable type.
	 *
	 * @param typeClass The variable type class name.
	 */
	public void setTypeClass(@Nullable String typeClass) {
		this.typeClass = typeClass;
	}

	/**
	 * Returns the value expression or source binding used for this variable.
	 *
	 * @return The variable value expression.
	 */
	public @Nullable String getValue() {
		return value;
	}

	/**
	 * Sets the value expression or source binding used for this variable.
	 *
	 * @param value The variable value expression.
	 */
	public void setValue(@Nullable String value) {
		this.value = value;
	}

	/**
	 * Renders this variable descriptor as a JRXML variable fragment.
	 *
	 * @return JRXML for this variable.
	 */
	public @Nonnull String getJrxml() {
		return Renderer.renderVariable(this);
	}

	/**
	 * Returns the parent design specification that owns this variable.
	 *
	 * @return The parent design specification.
	 */
	public @Nullable DesignSpecification getParent() {
		return parent;
	}
	
	/**
	 * Sets the parent design specification that owns this variable.
	 *
	 * @param parent The parent design specification.
	 */
	public void setParent(@Nullable DesignSpecification parent) {
		this.parent = parent;
	}
}
