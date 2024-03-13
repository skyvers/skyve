package org.skyve.metadata.view.model.list;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.JSON;
import org.skyve.util.Util;

/**
 * An in-memory list model that can generate the appropriate select statement to join across static and dynamic references and fields
 * using a MetaDataQuery as input.
 * @author mike
 *
 * @param <T>
 * 
 * The result query will look something ....
 * 
 * 				// dynamicAggregatedAssociation
 *				"left join ADM_DynamicRelation t1 on t1.parent_id = t0.bizId and t1.attributeName = 'dynamicAggregatedAssociation' " +
 *				"left join ADM_DynamicEntity t2 on t2.bizId = t1.relatedId " +
 *				// dynamicAggregatedAssociation.dynamicAggregatedAssociation
 *				"left join ADM_DynamicRelation t3 on t3.parent_id = t2.bizId and t3.attributeName = 'dynamicAggregatedAssociation' " +
 *				"left join ADM_DynamicEntity t4 on t4.bizId = t3.relatedId " +
 *				// aggregatedAssociation.bizKey (static)
 *				"left join ADM_DynamicRelation t5 on t5.parent_id = t0.bizId and t5.attributeName = 'aggregatedAssociation' " +
 *				"left join TEST_AllDynamicAttributesPersistent t6 on t6.bizId = t5.relatedId " +
 *				// aggregatedAssociation.colour (dynamic) (note using same DynamicRelation)
 *				"left join ADM_DynamicEntity t7 on t7.bizId = t5.relatedId " +
 *				// aggregatedAssociation.aggregatedAssociation.bizKey (static)
 *				"left join TEST_AllDynamicAttributesPersistent t8 on t8.bizId = t6.aggregatedAssociation_id " +
 *				"where t0.moduleName = :moduleName and t0.documentName = :documentName order by t0.bizId"		
 */
public class RDBMSDynamicPersistenceListModel<T extends Bean> extends InMemoryListModel<T> {
	private String description;
	private List<MetaDataQueryColumn> columns;
	private User user;
	private Customer customer;
	private Module module;
	private Document document;

	public void setQuery(MetaDataQueryDefinition query) throws Exception {
		description = query.getDescription();
		columns = query.getColumns();
		
		user = CORE.getUser();
		customer = user.getCustomer();
		module = query.getDocumentModule(customer);
		document = module.getDocument(customer, query.getDocumentName());

		setDrivingDocument(module, document);
	}
	
	public void setModel(String description, Document drivingDocument, List<MetaDataQueryColumn> columns) {
		this.description = description;
		this.columns = columns;
		
		user = CORE.getUser();
		customer = user.getCustomer();
		document = drivingDocument;
		module = customer.getModule(document.getOwningModuleName());

		setDrivingDocument(module, document);
	}
	
	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new UnsupportedOperationException("Update Not Supported");
	}

	@Override
	public void remove(String bizId) throws Exception {
		throw new UnsupportedOperationException("Remove Not Supported");
	}

	// Local structural object to hold "reference" table metadata
	private static class TableInfo {
		private Integer dynamicEntityTableAliasNumber; // alias number or null if not required
		private Integer dynamicRelationTableAliasNumber; // alias number or null if not required
		private Integer staticTableAliasNumber; // aslian number or null if not required
		private Integer fieldsIndex; // tuple index for the "fields" JSON value if it was projected
		private Document relatedDocument; // the document for this relation
	}
	
	private static class FieldInfo {
		private int fieldIndex;
		private Class<?> fieldType;
		
		private FieldInfo(int fieldIndex, Class<?> fieldType) {
			this.fieldIndex = fieldIndex;
			this.fieldType = fieldType;
		}
	}
	
	// Reference binding -> Table Info (see directly above)
	// A reference binding of "" is used for the driving document
	private Map<String, TableInfo> referenceBindingToTableInfo = new TreeMap<>();
	// Projection binding -> Tuple field index
	// A project may point to a "fields" field in which case it is read out of the JSON Map in the tuple.
	private Map<String, FieldInfo> projectionBindingToFieldInfo = new TreeMap<>();
	
	// The SQL projection clause
	private StringBuilder projectedColumns = new StringBuilder(128);
	// The SQL from clause
	private StringBuilder joinedTables = new StringBuilder(128);
	
	/**
	 * Performs the query but also servers as an overriding point for extension classes.
	 * @param projectedColumns
	 * @param joinedTables
	 * @return	the results.
	 */
	public AutoClosingIterable<Object[]> query(@SuppressWarnings("hiding") StringBuilder projectedColumns, @SuppressWarnings("hiding") StringBuilder joinedTables) {
		Persistence p = CORE.getPersistence();
		StringBuilder sql = new StringBuilder(projectedColumns.length() + joinedTables.length() + 90);
		sql.append("select ").append(projectedColumns).append(" from ").append(joinedTables);
		sql.append(" where t0.moduleName = :moduleName and t0.documentName = :documentName");
		return p.newSQL(sql.toString()).putParameter("moduleName", module.getName(), false).putParameter("documentName", document.getName(), false).tupleIterable();
	}
	
	@Override
	public List<Bean> getRows() throws Exception {
		// Populate the selectClause and fromClause and all the resulting table info
		prepare();
		
		// Execute the SQL generated above
		List<Bean> result = new ArrayList<>(128);
		try (AutoClosingIterable<Object[]> i = query(projectedColumns, joinedTables)) {
			// For each row...
			for (Object[] tuple : i) {
				// Convert all "fields" fields to a map and place back in the tuple
				// If a "fields" field is projected the table info's fieldsIndex will be defined.
				for (TableInfo info : referenceBindingToTableInfo.values()) {
					Integer fieldsIndexInteger = info.fieldsIndex;
					if (fieldsIndexInteger != null) {
						int fieldsIndex = fieldsIndexInteger.intValue();
						String json = (String) tuple[fieldsIndex];
						if (json != null) {
							tuple[fieldsIndex] = JSON.unmarshall(user, json);
						}
					}
				}
				
				boolean canFlag = false;
				if (user.canFlag()) {
					canFlag = true;
				}
				
				// Now extract the values out of the tuple.
				Map<String, Object> values = new TreeMap<>();
				for (Entry<String, FieldInfo> entry : projectionBindingToFieldInfo.entrySet()) {
					String projection = entry.getKey();
					// NB Set bizTagged null always
					Object value = PersistentBean.TAGGED_NAME.equals(projection) ? null : tuple[entry.getValue().fieldIndex];
					
					// Nullify flag comments if not given permissions
					if (! canFlag) {
						value = PersistentBean.FLAG_COMMENT_NAME.equals(projection) ? null : tuple[entry.getValue().fieldIndex];
					}

					// If we have a map we know we need to get the binding out of that map (its a "fields" field)
					if (value instanceof Map) {
						@SuppressWarnings("unchecked")
						Map<String, Object> fields = (Map<String, Object>) value;
						String simpleBinding = projection.substring(projection.lastIndexOf('.') + 1);
						value = fields.get(simpleBinding);
					}

					Class<?> fieldType = entry.getValue().fieldType;
					if ((value != null) && (! fieldType.equals(value.getClass()))) {
						try {
							value = BindUtil.fromSerialised(fieldType, value.toString());
						}
						catch (Exception e) {
							Document d = getDrivingDocument();
							Util.LOGGER.warning("RDBMSDynamicPersistenceListModel: Schema evolution problem on projection of binding " + projection + " within document " + d.getOwningModuleName() + "." + d.getName() + " :- [" + value + "] cannot be coerced to type " + fieldType);
							e.printStackTrace();
						}
					}
					values.put(projection, value);
				}
				result.add(new DynamicBean(module.getName(), document.getName(), values));
			}
		}

		return result;
	}
	
	// Used to make unique table aliases
	private int tableAliasNumber = 1; // driving table aliased to "t0"
	// Used to make unique field aliases (never used as we extract by position)
	private int fieldAliasNumber = 0; // nothing aliased at the start
	// The position in each tuple array
	private int fieldIndex = 4; // 4 implicit fields projected by default
	
	private void prepare() {
		// add alias zero - the driving table
		TableInfo info = new TableInfo();
		info.dynamicEntityTableAliasNumber = Integer.valueOf(0);
		referenceBindingToTableInfo.put("", info);
		// First 4 fields are the implicit fields required by any data model
		projectedColumns.append("t0.bizId, t0.bizLock, t0.bizFlagComment, t0.bizKey");
		joinedTables.append("ADM_DynamicEntity t0");
		
		// Now process each projection
		Set<String> projections = getProjections();
		for (String projection : projections) {
			// If we already have this one don't process it again
			if (projectionBindingToFieldInfo.containsKey(projection)) {
				continue;
			}
			
			// If we have a simple binding then we can run processing off of the driving table
			String[] simpleBindings = projection.split("\\.");
			if (simpleBindings.length == 1) { 
				// Take care of default projected implicit fields
				if (Bean.DOCUMENT_ID.equals(projection)) {
					projectionBindingToFieldInfo.put(projection, new FieldInfo(0, String.class));
				}
				else if (PersistentBean.LOCK_NAME.equals(projection)) {
					projectionBindingToFieldInfo.put(projection, new FieldInfo(1, OptimisticLock.class));
				}
				else if (PersistentBean.FLAG_COMMENT_NAME.equals(projection)) {
					projectionBindingToFieldInfo.put(projection, new FieldInfo(2, String.class));
				}
				else if (Bean.BIZ_KEY.equals(projection)) {
					projectionBindingToFieldInfo.put(projection, new FieldInfo(3, String.class));
				}
				else if (! PersistentBean.TAGGED_NAME.equals(projection)) { // not bizTagged
					// NB driving document is always dynamic
					
					// Add implicit field projections
					if (BindUtil.isImplicit(projection)) {
						projectedColumns.append(", t0.").append(projection).append(" as f").append(fieldAliasNumber++);
						projectionBindingToFieldInfo.put(projection, new FieldInfo(fieldIndex++, BindUtil.implicitAttributeType(projection)));
					}
					else {
						// Process simple references
						Attribute a = document.getPolymorphicAttribute(customer, projection);
						if (a instanceof Relation) {
							prepareReferences(simpleBindings);
							info = referenceBindingToTableInfo.get(projection);
							processProjectionThroughReferences(info, projection, Bean.DOCUMENT_ID, false, false, null);
						}
						// Process anything else
						else {
							info = referenceBindingToTableInfo.get("");
							if (info.fieldsIndex == null) {
								info.fieldsIndex = Integer.valueOf(fieldIndex++);
								projectedColumns.append(", t0.fields as f").append(fieldAliasNumber++);
							}
							addField(a, projection, info.fieldsIndex.intValue());
						}
					}
				}
			}
			// Compound bindings
			else {
				// Join all the tables required
				prepareReferences(simpleBindings);
				
				// Prepare the ultimate binding
				String prefix = projection.substring(0, projection.lastIndexOf('.'));
				info = referenceBindingToTableInfo.get(prefix);
				String simpleBinding = simpleBindings[simpleBindings.length - 1];
				Document relatedDocument = info.relatedDocument;
				Module relatedModule = customer.getModule(relatedDocument.getOwningModuleName());
				Attribute a = info.relatedDocument.getPolymorphicAttribute(customer, simpleBinding);
				processProjectionThroughReferences(info, projection, simpleBinding, BindUtil.isDynamic(customer, relatedModule, relatedDocument, a), a instanceof Relation, a);
			}
		}
	}
	
	/**
	 * Look at each simple binding and join in the appropriate table(s)
	 * @param simpleBindings
	 */
	private void prepareReferences(String[] simpleBindings) {
		Module simpleBindingModule = module;
		Document simpleBindingDocument = document;
		
		// look at all simple bindings in the binding expression
		String prefix = ""; // this keeps track of the expression we have already iterated over
		for (int i = 0, l = simpleBindings.length; i < l; i++) {
			String simpleBinding = simpleBindings[i];
			Attribute a = simpleBindingDocument.getPolymorphicAttribute(customer, simpleBinding);
			// If we have a relation here, determine how to join it in
			if (a instanceof Relation) {
				// Get the previous (owner) table info before adding to the prefix
				TableInfo ownerInfo = referenceBindingToTableInfo.get(prefix);

				// See if we've already processed this binding expression before
				prefix = (i == 0) ? simpleBinding : prefix + '.' + simpleBinding;
				if (! referenceBindingToTableInfo.containsKey(prefix)) {
					// The new table info we are going to add
					TableInfo info = new TableInfo();

					// Add the related document in - it had better be persistent
					Document relatedDocument = simpleBindingModule.getDocument(customer, ((Relation) a).getDocumentName());
					info.relatedDocument = relatedDocument;
					Persistent persistent = relatedDocument.getPersistent();
					if (persistent == null) {
						throw new DomainException("Can't join to a non-persistent document " + relatedDocument.getOwningModuleName() + '.' + relatedDocument.getName());
					}

					// Add dynamic relation left join, if applicable
					if (Binder.isDynamic(customer, simpleBindingModule, simpleBindingDocument, a)) {
						info.dynamicRelationTableAliasNumber = Integer.valueOf(tableAliasNumber);
						joinedTables.append("\nleft join ADM_DynamicRelation t").append(tableAliasNumber);
						joinedTables.append(" on t").append(tableAliasNumber).append(".parent_id = t").append(ownerInfo.dynamicEntityTableAliasNumber.intValue()).append(".bizId and t");
						joinedTables.append(tableAliasNumber).append(".attributeName = '").append(simpleBinding);
						tableAliasNumber++;

						// If relation target document is dynamic join ADM_DynamicEntity
						if (relatedDocument.isDynamic()) {
							// Add entity left join
							joinedTables.append("'\nleft join ADM_DynamicEntity t").append(tableAliasNumber);
							joinedTables.append(" on t").append(tableAliasNumber).append(".bizId = t").append(tableAliasNumber - 1).append(".relatedId");
	
							info.dynamicEntityTableAliasNumber = Integer.valueOf(tableAliasNumber);
						}
						// Else relation target is static - join in the persistent identifier
						else {
							// Add entity left join
							joinedTables.append("'\nleft join ").append(persistent.getPersistentIdentifier()).append(" t").append(tableAliasNumber);
							joinedTables.append(" on t").append(tableAliasNumber).append(".bizId = t").append(info.dynamicRelationTableAliasNumber).append(".relatedId");
							info.staticTableAliasNumber = Integer.valueOf(tableAliasNumber);
						}
						
						tableAliasNumber++;
						referenceBindingToTableInfo.put(prefix, info);

						// If we are at the end of our binding, check if we need the corresponding static/dynamic table for this attribute
						if (i == (l - 1)) { // last binding
							addStaticDynamicCounterpart(ownerInfo, true);
						}
					}
					// Its a static relation
					else {
						// Add entity left join
						joinedTables.append("\nleft join ").append(persistent.getPersistentIdentifier()).append(" t").append(tableAliasNumber);
						joinedTables.append(" on t").append(tableAliasNumber).append(".bizId = t").append(ownerInfo.staticTableAliasNumber.intValue()).append('.').append(simpleBinding).append("_id");
						info.staticTableAliasNumber = Integer.valueOf(tableAliasNumber);

						tableAliasNumber++;
						referenceBindingToTableInfo.put(prefix, info);

						// If we are at the end of our binding, check if we need the corresponding static/dynamic table for this attribute
						if (i == (l - 1)) { // last binding
							addStaticDynamicCounterpart(info, false);
						}
					}
				}

				// Walk the module name and document name across this relation binding
				String documentName = ((Relation) a).getDocumentName();
				simpleBindingDocument = simpleBindingModule.getDocument(customer, documentName);
				simpleBindingModule = customer.getModule(simpleBindingDocument.getOwningModuleName());
			}
			else if (a != null) { // last attribute is a scalar field (not implicit)
				// If the last simple binding part is not a relation, check if we need the corresponding static/dynamic table for this attribute
				TableInfo ownerInfo = referenceBindingToTableInfo.get(prefix);
				boolean dynamicAttribute = Binder.isDynamic(customer, simpleBindingModule, simpleBindingDocument, a);
				addStaticDynamicCounterpart(ownerInfo, dynamicAttribute);
			}
		}
	}
	
	/**
	 * Project the correct SQL expression based on the simple binding at the end of a portential compound expression.
	 * 
	 * @param info
	 * @param projection
	 * @param simpleBinding
	 * @param dynamicAttribute
	 * @param relationAttribute
	 */
	private void processProjectionThroughReferences(TableInfo info, String projection, String simpleBinding, boolean dynamicAttribute, boolean relationAttribute, Attribute a) {
		// Implicit static/dynamic attribute
		if (BindUtil.isImplicit(simpleBinding)) {
			projectedColumns.append(", t").append(info.relatedDocument.isDynamic() ? info.dynamicEntityTableAliasNumber : info.staticTableAliasNumber).append('.').append(simpleBinding).append(" as f").append(fieldAliasNumber++);
			projectionBindingToFieldInfo.put(projection, new FieldInfo(fieldIndex++, BindUtil.implicitAttributeType(simpleBinding)));
		}
		else {
			// Select from the "fields" field on the dynamic table if a dynamic attribute or a dynamic document
			if (dynamicAttribute || (info.relatedDocument.isDynamic())) {
				if (info.fieldsIndex == null) {
					info.fieldsIndex = Integer.valueOf(fieldIndex++);
					projectedColumns.append(", t").append(info.dynamicEntityTableAliasNumber).append(".fields as f").append(fieldAliasNumber++);
				}
				addField(a, projection, info.fieldsIndex.intValue());
			}
			// Select from the static table if a static attribute on a static document
			else {
				projectedColumns.append(", t").append(info.staticTableAliasNumber).append('.').append(simpleBinding);
				if (relationAttribute) {
					projectedColumns.append("_id");
				}
				projectedColumns.append(" as f").append(fieldAliasNumber++);
				addField(a, projection, fieldIndex++);
			}
		}
	}
	
	private void addField(Attribute a, String projection, int index) {
		Class<?> type = String.class;
		if (a instanceof Enumeration) {
			Enumeration e = (Enumeration) a;
			e = e.getTarget();
			if (e.isDynamic()) {
				type = String.class;
			}
			else {
				type = e.getEnum();
			}
		}
		else if (a != null) {
			type = a.getAttributeType().getImplementingType();
		}
		projectionBindingToFieldInfo.put(projection, new FieldInfo(index, type));
	}

	/**
	 * If we are projecting a dynamic attribute on a static document, ensure ADM_DynamicEntity is also joined in.
	 * If we are projecting a static attribute on a dynamic document, ensure the Persistent Identifier is joined in.
	 * 
	 * @param info
	 * @param dynamicAttribute
	 */
	private void addStaticDynamicCounterpart(TableInfo info, boolean dynamicAttribute) {
		// If the binding is dynamic then ensure that the penultimate relation has a dynamic entity table joined in 
		if (dynamicAttribute) {
			if ((info.staticTableAliasNumber != null) && (info.dynamicEntityTableAliasNumber == null)) {
				// Add dynamic entity left join from static table
				joinedTables.append("\nleft join ADM_DynamicEntity t").append(tableAliasNumber);
				joinedTables.append(" on t").append(tableAliasNumber).append(".bizId = t").append(info.staticTableAliasNumber).append(".bizId");

				info.dynamicEntityTableAliasNumber = Integer.valueOf(tableAliasNumber);
				tableAliasNumber++;
			}
		}
		// If the binding is static then ensure that the penultimate relation has the static table joined in 
		else {
			if ((info.dynamicEntityTableAliasNumber != null) && (info.staticTableAliasNumber == null)) {
				Persistent persistent = info.relatedDocument.getPersistent();
				if (persistent == null) {
					throw new DomainException("Can't join to a non-persistent document " + info.relatedDocument.getOwningModuleName() + '.' + info.relatedDocument.getName());
				}
				// Add static table left join from dynamic entity
				joinedTables.append("\nleft join ").append(persistent.getPersistentIdentifier()).append(" t").append(tableAliasNumber);
				joinedTables.append(" on t").append(info.dynamicEntityTableAliasNumber).append(".bizId = t").append(tableAliasNumber).append(".bizId");
				
				info.staticTableAliasNumber = Integer.valueOf(tableAliasNumber);
				tableAliasNumber++;
			}
		}
	}
}
