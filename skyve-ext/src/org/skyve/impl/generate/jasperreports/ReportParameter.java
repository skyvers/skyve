package org.skyve.impl.generate.jasperreports;

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
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getTypeClass() {
		return typeClass;
	}
	public void setTypeClass(String typeClass) {
		this.typeClass = typeClass;
	}
	public String getDefaultValueExpression() {
		return defaultValueExpression;
	}
	public void setDefaultValueExpression(String defaultValueExpression) {
		this.defaultValueExpression = defaultValueExpression;
	}
	public String getJrxml() {
		return Renderer.renderParameter(this);
	}
	public DesignSpecification getParent() {
		return parent;
	}
	public void setParent(DesignSpecification parent) {
		this.parent = parent;
	}
}
