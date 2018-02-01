package org.skyve.impl.generate.jasperreports;

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
	public String getValue() {
		return value;
	}
	public void setValue(String value) {
		this.value = value;
	}
	public String getJrxml() {
		return Renderer.renderVariable(this);
	}
	public DesignSpecification getParent() {
		return parent;
	}
	public void setParent(DesignSpecification parent) {
		this.parent = parent;
	}
}
