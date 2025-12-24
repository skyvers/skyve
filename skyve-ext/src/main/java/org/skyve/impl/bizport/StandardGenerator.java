package org.skyve.impl.bizport;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.skyve.bizport.BizPortColumn;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.bizport.SheetKey;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.util.BeanVisitor;
import org.skyve.util.NullableBeanVisitor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Utility class to generate a spreadsheet shape from a document definition.
 * 
 * @author mike
 *
 */
public final class StandardGenerator {

    private static final Logger LOGGER = LoggerFactory.getLogger(StandardGenerator.class);

	/**
	 * The relevant customer for the generation.
	 */
	private @Nonnull Customer customer;
	
	/**
	 * The driving document for the generation.
	 */
	private @Nonnull Document document;
	
	/**
	 * Bindings (relative to driving document) to exclude from the generation.
	 * Each binding will stop the recursive generation processing when it is satisfied.
	 */
	private @Nonnull String[] exclusions;

	/**
	 * 
	 * @param customer	The relevant customer for the generation.
	 * @param document	The driving document for the generation.
	 * @param exclusions	Bindings (relative to driving document) to exclude from the generation.
	 * 						Each binding will stop the recursive generation processing when it is satisfied.
	 */
	public StandardGenerator(@Nonnull Customer customer, @Nonnull Document document, @Nonnull String... exclusions) {
		this.customer = customer;
		this.document = document;
		this.exclusions = exclusions;
	}

	/**
	 * Generate a structure which will accomodate the driving document for the customer
	 * into the given workbook.
	 * 
	 * @param workbook	The workbook to generate the structure into.
	 */
	public void generateStructure(@Nonnull final BizPortWorkbook workbook) {
		new NullableBeanVisitor(false, false) {
			// processBean can be null as we are visiting ALL
			@Override
			protected boolean acceptNulls(String binding,
											Document processDocument,
											Document owningDocument,
											Relation owningRelation,
											Bean processBean) throws Exception {
				LOGGER.info("B = {}", binding);

				// stop recursive processing if we have matched an exclusion
				for (String exclusion : exclusions) {
					if (binding.startsWith(exclusion)) {
						return false;
					}
				}

				// if its a collection, put in a joining sheet
				if (owningRelation instanceof Collection collection) {
					Collection owningCollection = collection;
					if (! CollectionType.child.equals(owningCollection.getType())) {
						BizPortSheet collectionSheet = workbook.getSheet(new SheetKey(document.getOwningModuleName(), 
																						document.getName(),
																						binding));
						if (collectionSheet == null) {
							// lower levels
							generateAndAddCollectionSheet(binding, 
															owningDocument, 
															owningCollection, 
															processDocument, 
															workbook);
						}
					}
				}

				// ensure that there is a document sheet created, if there isn't one already
				boolean recurse = (workbook.getSheet(new SheetKey(processDocument.getOwningModuleName(), 
																	processDocument.getName())) == null);
				if (recurse) {
					// lower levels
					boolean embeddedAssociation = (owningRelation instanceof Association association) && 
													AssociationType.embedded.equals((association).getType());
					if (! embeddedAssociation) {
						generateAndAddDocumentSheet(processDocument, owningRelation, workbook);
					}
				}

				return recurse;
			}
		}.visit(document, null, customer);
	}

	/**
	 * Fill the workbook with data given a bean of the driving document type.
	 * If the bean is a hierarchical bean then its children are included.
	 * @param workbook	The workbook to fill.
	 * @param beans	The data to use to fill with.
	 */
	public void generateData(@Nonnull final BizPortWorkbook workbook, @Nonnull final Bean bean) {
		// check if hierarchical, and include children in export
		Module m = customer.getModule(bean.getBizModule());
		String documentName = bean.getBizDocument();
		Document d = m.getDocument(customer, documentName);
		String parentDocumentName = d.getParentDocumentName();
		if (documentName.equals(parentDocumentName)) { // hierarchical
			Set<Bean> nodes = new LinkedHashSet<>();
			collect(bean, nodes);
			generateData(workbook, nodes);
		}
		else { // not hierarchical - just the single bean to export
			generateData(workbook, Collections.singleton(bean));
		}
	}
	
	// Grab all nodes of the hierarchy and add to the "nodes" set
	private void collect(@Nonnull Bean bean, @Nonnull Set<Bean> nodes) {
		nodes.add(bean);
		if (bean instanceof HierarchicalBean<?> hierarchicalBean) {
			@SuppressWarnings("unchecked")
			HierarchicalBean<? extends Bean> node = hierarchicalBean;
			for (Bean child : node.getChildren()) {
				collect(child, nodes);
			}
		}
	}
	
	/**
	 * Fill the workbook with data given a list of beans of the driving document type.
	 * @param workbook	The workbook to fill.
	 * @param beans	The data to use to fill with.
	 */
	public void generateData(@Nonnull final BizPortWorkbook workbook, 
								@Nonnull final Iterable<? extends Bean> beans) {
		// Recursively walks the topBean's object graph populating the relevant 
		// sheets in the workbook.
		BeanVisitor excelBeanVisitor = new BeanVisitor(false, false) {
			// the top-most bean to process
			private Bean topBean;

			@Override
			protected boolean accept(String binding,
										Document currentDocument,
										Document owningDocument,
										Relation owningRelation,
										Bean bean) throws Exception {
				if ("".equals(binding)) { // top level
					topBean = bean;
				}

				String bizId = bean.getBizId();

				final String currentDocumentName = currentDocument.getName();
				SheetKey key = null;
				boolean orderedChildCollection = false;

				// Add bean to the collection sheet if required
				if (owningRelation instanceof Collection collection) {
					Collection owningCollection = collection;
					if (CollectionType.child.equals(owningCollection.getType())) {
						key = new SheetKey(currentDocument.getOwningModuleName(), currentDocumentName, owningCollection.getName());
						orderedChildCollection = Boolean.TRUE.equals(owningCollection.getOrdered());
					}
					else {
						// search for the collection sheets through the binding with the array indexing removed - eg [0], [1]
						String collectionBinding = binding.replaceAll("\\[\\d*\\]", "");
						BizPortSheet collectionSheet = workbook.getSheet(new SheetKey(document.getOwningModuleName(), 
																						document.getName(), 
																						collectionBinding));
						// collectionSheet can be null if the binding was struck out
						// during the spreadsheet generation (may be in the exclusions),
						// or was deleted from the workbook by the user
						if (collectionSheet != null) {
							// determine the owner id
							String ownerId = null;
							int dotIndex = binding.lastIndexOf('.');
							if (dotIndex < 0) { // no dot
								ownerId = topBean.getBizId();
							}
							else {
								String ownerBinding = binding.substring(0, dotIndex);
								Bean ownerBean = (Bean) BindUtil.get(topBean, ownerBinding);
								if (ownerBean == null) {
									throw new IllegalStateException("ownerBean is null");
								}
								ownerId = ownerBean.getBizId();
							}

							collectionSheet.addRow(ownerId + bizId);
							collectionSheet.setValue(PersistentBean.OWNER_COLUMN_NAME, ownerId);
							collectionSheet.setValue(PersistentBean.ELEMENT_COLUMN_NAME, bizId);
						}
					}
				}

				// Add bean attributes
				if (key == null) { // not a child collection
					key = new SheetKey(currentDocument.getOwningModuleName(), currentDocumentName);
				}
				BizPortSheet sheet = workbook.getSheet(key);
				// SheetData can be null if all bindings relating to this type were struck out
				// during the template creation (may be in the exclusions)
				// or the user deleted the sheet out of the workbook manually.
				if ((sheet != null) && (! sheet.moveToRow(bizId))) {
					sheet.addRow(bizId);

					Set<String> columnBindings = sheet.getColumnBindings();
					
					// add the ID first
					sheet.setValue(Bean.DOCUMENT_ID, bizId);
					if (columnBindings.contains(Bean.BIZ_KEY) && 
							(bean instanceof PersistentBean persistentBean)) {
						sheet.setValue(Bean.BIZ_KEY, (persistentBean).getBizKey());
					}

					generateRowData(sheet, columnBindings, currentDocument, bean, null);
					
					// Parent ID column
					final String parentDocumentName = currentDocument.getParentDocumentName();
					if (parentDocumentName != null) {
						if (parentDocumentName.equals(currentDocumentName)) {
							if (columnBindings.contains(HierarchicalBean.PARENT_ID)) {
								sheet.setValue(HierarchicalBean.PARENT_ID, 
												BindUtil.get(bean, HierarchicalBean.PARENT_ID));
							}

						}
						else {
							if (columnBindings.contains(ChildBean.PARENT_NAME)) {
								sheet.setValue(ChildBean.PARENT_NAME, 
												BindUtil.get(bean, new StringBuilder(32).append(ChildBean.PARENT_NAME).append('.').append(Bean.DOCUMENT_ID).toString()));
							}
							
							if (orderedChildCollection && columnBindings.contains(Bean.ORDINAL_NAME)) {
								sheet.setValue(Bean.ORDINAL_NAME, BindUtil.get(bean, Bean.ORDINAL_NAME));
							}
						}
					}
				}

				return true;
			}
		};

		for (Bean bean : beans) {
			excelBeanVisitor.visit(document, bean, customer);
		}
	}

	private void generateRowData(@Nonnull BizPortSheet sheet,
									@Nonnull Set<String> columnBindings,
									@Nonnull Document currentDocument,
									@Nonnull Bean bean,
									@Nullable String embeddedAssociationName) {
		for (Attribute attribute : currentDocument.getAllAttributes(customer)) {
			AttributeType type = attribute.getAttributeType();
			String name = attribute.getName();
			String columnBinding = name;
			if (embeddedAssociationName != null) {
				columnBinding = new StringBuilder(64).append(embeddedAssociationName).append('.').append(columnBinding).toString();
			}
			String valueBinding = columnBinding;
			
			// Process if not a collection and not the bizKey
			if ((! AttributeType.collection.equals(type)) && (! Bean.BIZ_KEY.equals(name))) {
				if (attribute instanceof Association association) {
					final Module owningModule = customer.getModule(currentDocument.getOwningModuleName());
					final Document associationDocument = owningModule.getDocument(customer, association.getDocumentName());
					
					if (AssociationType.embedded.equals(association.getType())) { // embedded
						generateRowData(sheet, columnBindings, associationDocument, bean, name);
					}
					else {
						if (columnBindings.contains(columnBinding)) {
							valueBinding = new StringBuilder(64).append(valueBinding).append('.').append(Bean.DOCUMENT_ID).toString();
							sheet.setValue(columnBinding, BindUtil.get(bean, valueBinding));
						}
					}
				}
				else {
					if (columnBindings.contains(columnBinding)) {
						sheet.setValue(columnBinding, BindUtil.get(bean, valueBinding));
					}
				}
			}
		}
	}
	
	/**
	 * Make a sheet for a document encountered.
	 * The sheet is added to the workbook under the owning module name and document name.
	 * @param currentDocument	The document to generate for.
	 * @param workbook	The workbook to generate in.
	 * @throws Exception
	 */
	private void generateAndAddDocumentSheet(@Nonnull Document currentDocument,
												@Nullable Relation owningRelation,
												@Nonnull BizPortWorkbook workbook) 
	throws Exception {
		BizPortSheet sheet = new POISheet(currentDocument.getLocalisedSingularAlias());

		// ID column
		BizPortColumn column = new BizPortColumn("ID",
													"The surrogate key.  This can be an existing ID for an existing object or any unique number or name for a new object.",
													AttributeType.text);
		sheet.addColumn(Bean.DOCUMENT_ID, column);

		// BizKey column
		column = new BizPortColumn("Business Key",
										"The business description of the record.  This value is NOT uploaded but allows a referential description within the spreadsheet.", 
										AttributeType.text);
		sheet.addColumn(Bean.BIZ_KEY, column);
		
		final String currentDocumentName = currentDocument.getName();
		final String currentDocumentOwningModuleName = currentDocument.getOwningModuleName();

		generateAttributeColumns(sheet, currentDocument, null, null);
		
		// add the parent if this is a child document
		SheetKey key = null;
		final String parentDocumentName = currentDocument.getParentDocumentName();
		if (parentDocumentName != null) {
			if (parentDocumentName.equals(currentDocumentName)) {
				column = new BizPortColumn("Parent ID", "This points to the parent in a hiearachy within this shee", AttributeType.text);
				column.setReferencedSheet(new SheetKey(currentDocumentOwningModuleName, parentDocumentName));
				sheet.addColumn(HierarchicalBean.PARENT_ID, column);
			}
			else {
				Document parentDocument = currentDocument.getParentDocument(customer);
				if ((parentDocument != null) && (owningRelation instanceof Collection collection)) { // definitely a child document
					String localisedSingularAlias = parentDocument.getLocalisedSingularAlias();
					column = new BizPortColumn(localisedSingularAlias + " ID (Parent)",
													"The 'Parent' link of the relationship.  Populate this with " + localisedSingularAlias + " IDs.", 
													AttributeType.text);
					column.setReferencedSheet(new SheetKey(parentDocument.getOwningModuleName(), parentDocument.getName()));
					sheet.addColumn(ChildBean.PARENT_NAME, column);
		
					if (Boolean.TRUE.equals((collection).getOrdered())) {
						column = new BizPortColumn("Ordinal", "The order of these records", AttributeType.integer);
						sheet.addColumn(Bean.ORDINAL_NAME, column);
					}
					key = new SheetKey(currentDocumentOwningModuleName, currentDocumentName, owningRelation.getName());
					sheet.setTitle(owningRelation.getLocalisedDisplayName() + " (" + sheet.getTitle() + ")");
				}
			}
		}
		if (key == null) { // not a child document
			key = new SheetKey(currentDocumentOwningModuleName, currentDocumentName);
		}
		
		workbook.addSheet(key, sheet);
	}

	/**
	 * This method is used to add all scalar attributes to the given sheet, recursing when an embedded association is encountered.
	 * 
	 * @param sheet	The sheet to add columns to.
	 * @param currentDocument	The document to get the attributes for.
	 * @param embeddedAssociationName	If recursing an embedded association, its name. 
	 * @param embeddedAssociationDisplayName	If recursing an embedded association, its display name.
	 * @throws Exception
	 */
	private void generateAttributeColumns(@Nonnull BizPortSheet sheet,
											@Nonnull Document currentDocument,
											@Nullable String embeddedAssociationName,
											@Nullable String embeddedAssociationDisplayName)
	throws Exception {
		Bizlet<?> bizlet = null;

		final String currentDocumentName = currentDocument.getName();
		final String currentDocumentOwningModuleName = currentDocument.getOwningModuleName();
		for (Attribute attribute : currentDocument.getAllAttributes(customer)) {
			AttributeType type = attribute.getAttributeType();
			String name = attribute.getName();

			// Leave collections and biz keys out of it
			if (! ((attribute instanceof Collection) || Bean.BIZ_KEY.equals(name))) {
				BizPortColumn column = null; // the column to add

				if (attribute instanceof Association association) {
					final Module owningModule = customer.getModule(currentDocumentOwningModuleName);
					final Document associationDocument = owningModule.getDocument(customer, association.getDocumentName());
					final String displayName = attribute.getLocalisedDisplayName();
					
					if (AssociationType.embedded.equals(association.getType())) { // embedded - just add compound bindings to the current sheet
						generateAttributeColumns(sheet, associationDocument, name, displayName);
					}
					else { // aggregated or composed
						StringBuilder title = new StringBuilder(128);
						if (embeddedAssociationDisplayName != null) {
							title.append(embeddedAssociationDisplayName).append(' ');
						}
						title.append(displayName).append(" ID");
						column = new BizPortColumn(title.toString(), attribute.getLocalisedDescription(), type);
						column.setReferencedSheet(new SheetKey(associationDocument.getOwningModuleName(), 
																associationDocument.getName()));
					}
				}
				else {
					String comment = attribute.getLocalisedDescription();
					if (embeddedAssociationDisplayName != null) {
						StringBuilder title = new StringBuilder(128);
						title.append(embeddedAssociationDisplayName).append(' ').append(attribute.getLocalisedDisplayName());
						column = new BizPortColumn(title.toString(), comment, type);
					}
					else {
						column = new BizPortColumn(attribute.getLocalisedDisplayName(), comment, type);
					}
				}

				if (column != null) {
					if (DomainType.constant.equals(attribute.getDomainType())) {
						if (bizlet == null) {
							bizlet = currentDocument.getBizlet(customer);
						}
						if (bizlet == null) { // metadata has an error
							bizlet = new Bizlet<>() {
								// empty bizlet
							};
						}
						column.setRangeValues(getConstantRangeValues(bizlet, 
																		currentDocumentOwningModuleName,
																		currentDocumentName,
																		attribute));
					}
	
					if (embeddedAssociationName != null) {
						StringBuilder binding = new StringBuilder(64);
						binding.append(embeddedAssociationName).append('.');
						binding.append(name);
						sheet.addColumn(binding.toString(), column);
					}
					else {
						sheet.addColumn(name, column);
					}
				}
			}
		}
	}
	
	/**
	 * Make a sheet for a collection within the document encountered.
	 * The sheet is added to the workbook under the owning module name and document name and collection binding.
	 * @param binding	The binding of the collection relative to the owningDocument.
	 * @param owningDocument	The document that owns the collection.
	 * @param collection	The collection metadata.
	 * @param collectionDocument	The document type of the collection.
	 * @param workbook	The workbook to generate in.
	 */
	private void generateAndAddCollectionSheet(@Nonnull String binding,
												@Nonnull Document owningDocument,
												@Nonnull Collection collection,
												@Nonnull Document collectionDocument,
												@Nonnull BizPortWorkbook workbook) {
		BizPortSheet sheet = new POISheet(collection.getLocalisedDisplayName());

		// Owner ID column
		String localisedSingularAlias = owningDocument.getLocalisedSingularAlias();
		BizPortColumn column = new BizPortColumn(localisedSingularAlias + " ID (From)",
													"The 'From' link of the relationship.  Populate this with " + localisedSingularAlias + " IDs.",
													AttributeType.text);
		column.setReferencedSheet(new SheetKey(owningDocument.getOwningModuleName(), owningDocument.getName()));
		sheet.addColumn(PersistentBean.OWNER_COLUMN_NAME, column);

		// Element ID column
		localisedSingularAlias = collectionDocument.getLocalisedSingularAlias();
		column = new BizPortColumn(localisedSingularAlias + " ID (To)",
									"The 'To' link of the relationship.  Populate this with " + localisedSingularAlias + " IDs.",
									AttributeType.text);
		column.setReferencedSheet(new SheetKey(collectionDocument.getOwningModuleName(), collectionDocument.getName()));
		sheet.addColumn(PersistentBean.ELEMENT_COLUMN_NAME, column);

		if (Boolean.TRUE.equals(collection.getOrdered())) {
			column = new BizPortColumn("Ordinal", "The order of these records", AttributeType.integer);
			sheet.addColumn(Bean.ORDINAL_NAME, column);
		}

		workbook.addSheet(new SheetKey(document.getOwningModuleName(), document.getName(), binding), sheet);
	}

	/**
	 * Determine constant range values for the spreadsheet from the bizlet domain values.
	 * @param domainBizlet	The bizlet to get domain values from.
	 * @param domainModuleName	The module to get the domain values for.
	 * @param domainDocumentName	The document to get the domain values for.
	 * @param domainAttribute	The attribute to get domain values for.
	 * @return	An array of range values.
	 * @throws Exception
	 */
	private @Nonnull String[] getConstantRangeValues(@Nonnull Bizlet<?> domainBizlet,
														@Nonnull String domainModuleName,
														@Nonnull String domainDocumentName,
														@Nonnull Attribute domainAttribute) 
	throws Exception {
		List<DomainValue> values = customer.getConstantDomainValues(domainBizlet,
																		domainModuleName,
																		domainDocumentName,
																		domainAttribute);
		String[] result = new String[values.size()];
		int i = 0;
		for (DomainValue value : values) {
			result[i] = value.getCode();
			i++;
		}

		return result;
	}
}
