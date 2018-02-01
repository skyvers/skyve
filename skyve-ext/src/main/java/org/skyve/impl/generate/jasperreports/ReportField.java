package org.skyve.impl.generate.jasperreports;

public class ReportField  {
	/**
	 * Name
	 **/
	private String name;
	/**
	 * Class
	 **/
	private String typeClass;
	/**
	 * Include Total
	 **/
	private Boolean includeTotal;
	/**
	 * Implicit Field
	 **/
	private Boolean implicit;
	/**
	 * Skyve Type
	 **/
	private String skyveType;
	/**
	 * Display Name
	 **/
	private String displayName;
	/**
	 * Owning Module Name
	 **/
	private String owningModuleName;
	/**
	 * Document Name
	 **/
	private String documentName;
	/**
	 * Binding
	 **/
	private String binding;
	/**
	 * Name (SQL)
	 **/
	private String nameSql;
	/**
	 * Join
	 **/
	private String joinSql;
	/**
	 * Collection
	 **/
	private Boolean collection;
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
	public Boolean getIncludeTotal() {
		return includeTotal;
	}
	public void setIncludeTotal(Boolean includeTotal) {
		this.includeTotal = includeTotal;
	}
	public String getJrxml() {
		return Renderer.renderField(this);
	}
	public Boolean getImplicit() {
		return implicit;
	}
	public void setImplicit(Boolean implicit) {
		this.implicit = implicit;
	}
	public String getSkyveType() {
		return skyveType;
	}
	public void setSkyveType(String skyveType) {
		this.skyveType = skyveType;
	}
	public String getDisplayName() {
		return displayName;
	}
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}
	public String getOwningModuleName() {
		return owningModuleName;
	}
	public void setOwningModuleName(String owningModuleName) {
		this.owningModuleName = owningModuleName;
	}
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}
	public String getBinding() {
		return binding;
	}
	public void setBinding(String binding) {
		this.binding = binding;
	}
	public String getNameSql() {
		return nameSql;
	}
	public void setNameSql(String nameSql) {
		this.nameSql = nameSql;
	}
	public String getJoinSql() {
		return joinSql;
	}
	public void setJoinSql(String joinSql) {
		this.joinSql = joinSql;
	}
	public Boolean getCollection() {
		return collection;
	}
	public void setCollection(Boolean collection) {
		this.collection = collection;
	}
	public DesignSpecification getParent() {
		return parent;
	}
	public void setParent(DesignSpecification parent) {
		this.parent = parent;
	}
	public String getDocumentName() {
		return documentName;
	}
}
