package org.skyve.impl.generate.jasperreports;

/**
 * Descriptor for a single JasperReports field, holding the name, class, and
 * optional description used when generating a {@code .jrxml} programmatically.
 */
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

	/**
	 * Returns the field name used in Jasper field declarations.
	 *
	 * @return The field name.
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the field name used in Jasper field declarations.
	 *
	 * @param name The field name.
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Returns the Java class name used for the Jasper field type.
	 *
	 * @return The field type class name.
	 */
	public String getTypeClass() {
		return typeClass;
	}

	/**
	 * Sets the Java class name used for the Jasper field type.
	 *
	 * @param typeClass The field type class name.
	 */
	public void setTypeClass(String typeClass) {
		this.typeClass = typeClass;
	}

	/**
	 * Indicates whether this field should participate in generated totals.
	 *
	 * @return {@code TRUE} when totals should include this field, otherwise {@code FALSE} or {@code null}.
	 */
	public Boolean getIncludeTotal() {
		return includeTotal;
	}

	/**
	 * Sets whether this field should participate in generated totals.
	 *
	 * @param includeTotal {@code TRUE} to include in totals, otherwise {@code FALSE}.
	 */
	public void setIncludeTotal(Boolean includeTotal) {
		this.includeTotal = includeTotal;
	}

	/**
	 * Renders this field descriptor as a JRXML field fragment.
	 *
	 * @return JRXML for this field.
	 */
	public String getJrxml() {
		return Renderer.renderField(this);
	}

	/**
	 * Indicates whether this field is implicitly generated rather than explicitly selected.
	 *
	 * @return {@code TRUE} when implicit, otherwise {@code FALSE} or {@code null}.
	 */
	public Boolean getImplicit() {
		return implicit;
	}

	/**
	 * Sets whether this field is implicitly generated.
	 *
	 * @param implicit {@code TRUE} when the field is implicit.
	 */
	public void setImplicit(Boolean implicit) {
		this.implicit = implicit;
	}

	/**
	 * Returns the Skyve attribute type associated with this field.
	 *
	 * @return The Skyve attribute type.
	 */
	public String getSkyveType() {
		return skyveType;
	}

	/**
	 * Sets the Skyve attribute type associated with this field.
	 *
	 * @param skyveType The Skyve attribute type.
	 */
	public void setSkyveType(String skyveType) {
		this.skyveType = skyveType;
	}

	/**
	 * Returns the user-facing label for this field.
	 *
	 * @return The display name.
	 */
	public String getDisplayName() {
		return displayName;
	}

	/**
	 * Sets the user-facing label for this field.
	 *
	 * @param displayName The display name.
	 */
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	/**
	 * Returns the module that owns the source document.
	 *
	 * @return The owning module name.
	 */
	public String getOwningModuleName() {
		return owningModuleName;
	}

	/**
	 * Sets the module that owns the source document.
	 *
	 * @param owningModuleName The owning module name.
	 */
	public void setOwningModuleName(String owningModuleName) {
		this.owningModuleName = owningModuleName;
	}

	/**
	 * Sets the source document name for this field.
	 *
	 * @param documentName The document name.
	 */
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

	/**
	 * Returns the binding path used to resolve values for this field.
	 *
	 * @return The field binding path.
	 */
	public String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding path used to resolve values for this field.
	 *
	 * @param binding The field binding path.
	 */
	public void setBinding(String binding) {
		this.binding = binding;
	}

	/**
	 * Returns the SQL projection alias or expression used for this field name.
	 *
	 * @return The SQL field name expression.
	 */
	public String getNameSql() {
		return nameSql;
	}

	/**
	 * Sets the SQL projection alias or expression used for this field name.
	 *
	 * @param nameSql The SQL field name expression.
	 */
	public void setNameSql(String nameSql) {
		this.nameSql = nameSql;
	}

	/**
	 * Returns the SQL join fragment needed to source this field.
	 *
	 * @return The SQL join fragment.
	 */
	public String getJoinSql() {
		return joinSql;
	}

	/**
	 * Sets the SQL join fragment needed to source this field.
	 *
	 * @param joinSql The SQL join fragment.
	 */
	public void setJoinSql(String joinSql) {
		this.joinSql = joinSql;
	}

	/**
	 * Indicates whether this field represents a collection-valued attribute.
	 *
	 * @return {@code TRUE} when collection-valued, otherwise {@code FALSE} or {@code null}.
	 */
	public Boolean getCollection() {
		return collection;
	}

	/**
	 * Sets whether this field represents a collection-valued attribute.
	 *
	 * @param collection {@code TRUE} when collection-valued.
	 */
	public void setCollection(Boolean collection) {
		this.collection = collection;
	}

	/**
	 * Returns the parent design specification that owns this field.
	 *
	 * @return The parent design specification.
	 */
	public DesignSpecification getParent() {
		return parent;
	}

	/**
	 * Sets the parent design specification that owns this field.
	 *
	 * @param parent The parent design specification.
	 */
	public void setParent(DesignSpecification parent) {
		this.parent = parent;
	}
	
	/**
	 * Returns the source document name for this field.
	 *
	 * @return The document name.
	 */
	public String getDocumentName() {
		return documentName;
	}
}
