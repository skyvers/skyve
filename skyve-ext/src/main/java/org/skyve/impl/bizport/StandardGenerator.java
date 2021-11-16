package org.skyve.impl.bizport;

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
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.util.BeanVisitor;

/**
 * Utility class to generate a spreadsheet shape from a document definition.
 * 
 * @author mike
 *
 */
public final class StandardGenerator {
	/**
	 * The relevant customer for the generation.
	 */
	private Customer customer;
	
	/**
	 * The driving document for the generation.
	 */
	private Document document;
	
	/**
	 * Bindings (relative to driving document) to exclude from the generation.
	 * Each binding will stop the recursive generation processing when it is satisfied.
	 */
	private String[] exclusions = null;

	/**
	 * 
	 * @param customer	The relevant customer for the generation.
	 * @param document	The driving document for the generation.
	 * @param exclusions	Bindings (relative to driving document) to exclude from the generation.
	 * 						Each binding will stop the recursive generation processing when it is satisfied.
	 */
	public StandardGenerator(Customer customer, Document document, String... exclusions) {
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
	public void generateStructure(final BizPortWorkbook workbook) {
		new BeanVisitor(true, false, false) {
			// processBean can be null as we are visiting ALL
			@Override
			protected boolean accept(String binding,
										Document processDocument,
										Document owningDocument,
										Relation owningReference,
										Bean processBean) throws Exception {
				UtilImpl.LOGGER.info("B = " + binding);

				// stop recursive processing if we have matched an exclusion
				for (String exclusion : exclusions) {
					if (binding.startsWith(exclusion)) {
						return false;
					}
				}

				// if its a collection, put in a joining sheet
				if (owningReference instanceof Collection) {
					Collection owningCollection = (Collection) owningReference;
					if (! CollectionType.child.equals(owningCollection.getType())) {
						BizPortSheet collectionSheet = workbook.getSheet(new SheetKey(document.getOwningModuleName(), 
																						document.getName(), binding));
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

				// ensure that there is a document sheet created,
				// if there isn't one already
				boolean recurse = (workbook.getSheet(new SheetKey(processDocument.getOwningModuleName(), 
																	processDocument.getName())) == null);
				if (recurse) {
					// lower levels
					generateAndAddDocumentSheet(processDocument, workbook);
				}

				return recurse;
			}
		}.visit(document, null, customer);
	}

	/**
	 * Fill the workbook with data given a list of beans of the driving document type.
	 * @param workbook	The workbook to fill.
	 * @param beans	The data to use to fill with.
	 */
	public void generateData(final BizPortWorkbook workbook, 
								Iterable<? extends Bean> beans) {
		// Recursively walks the topBean's object graph populating the relevant 
		// sheets in the workbook.
		BeanVisitor excelBeanVisitor = new BeanVisitor(false, false, false) {
			// the top-most bean to process
			private Bean topBean;

			@Override
			protected boolean accept(String binding,
										Document currentDocument,
										Document owningDocument,
										Relation owningReference,
										Bean bean) throws Exception {
				if ("".equals(binding)) { // top level
					topBean = bean;
				}

				String bizId = bean.getBizId();

				// Add bean to the collection sheet if required
				if (owningReference instanceof Collection) {
					Collection owningCollection = (Collection) owningReference;
					if (! CollectionType.child.equals(owningCollection.getType())) {
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
								ownerId = ((Bean) BindUtil.get(topBean, ownerBinding)).getBizId();
							}

							collectionSheet.addRow(ownerId + bizId);
							collectionSheet.setValue(PersistentBean.OWNER_COLUMN_NAME, ownerId);
							collectionSheet.setValue(PersistentBean.ELEMENT_COLUMN_NAME, bizId);
						}
					}
				}

				// Add bean attributes
				BizPortSheet sheet = workbook.getSheet(new SheetKey(currentDocument.getOwningModuleName(), 
																		currentDocument.getName()));
				// SheetData can be null if all bindings relating to this type were struck out
				// during the template creation (may be in the exclusions)
				// or the user deleted the sheet out of the workbook manually.
				if ((sheet != null) && (! sheet.moveToRow(bizId))) {
					sheet.addRow(bizId);

					Set<String> columnBindings = sheet.getColumnBindings();
					
					// add the ID first
					sheet.setValue(Bean.DOCUMENT_ID, bizId);
					if (columnBindings.contains(Bean.BIZ_KEY) && 
							(bean instanceof PersistentBean)) {
						sheet.setValue(Bean.BIZ_KEY, ((PersistentBean) bean).getBizKey());
					}

					for (Attribute attribute : currentDocument.getAllAttributes()) {
						AttributeType type = attribute.getAttributeType();
						String name = attribute.getName();

						if ((! AttributeType.collection.equals(type)) && 
								(! Bean.BIZ_KEY.equals(name)) &&
								columnBindings.contains(name)) {
							if (AttributeType.association.equals(type)) {
								name = new StringBuilder(32).append(name).append('.').append(Bean.DOCUMENT_ID).toString();
							}
							sheet.setValue(attribute.getName(), BindUtil.get(bean, name));
						}
					}

					// Parent ID column
					if (currentDocument.getParentDocumentName() != null) {
						if (currentDocument.getParentDocumentName().equals(currentDocument.getName())) {
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
							
							Document parentDocument = currentDocument.getParentDocument(customer);
							Reference reference = ((DocumentImpl) parentDocument).getReferenceByDocumentName(currentDocument.getName());
							if ((reference instanceof Collection) && Boolean.TRUE.equals(((Collection) reference).getOrdered()) &&
									columnBindings.contains(Bean.ORDINAL_NAME)) {
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

	/**
	 * Make a sheet for a document encountered.
	 * The sheet is added to the workbook under the owning module name and document name.
	 * @param currentDocument	The document to generate for.
	 * @param workbook	The workbook to generate in.
	 * @throws Exception
	 */
	private void generateAndAddDocumentSheet(Document currentDocument, BizPortWorkbook workbook) 
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
		
		Bizlet<?> bizlet = null;

		for (Attribute attribute : currentDocument.getAllAttributes()) {
			AttributeType type = attribute.getAttributeType();
			String name = attribute.getName();

			// Leave collections and biz keys out of it
			if (! ((attribute instanceof Collection) || Bean.BIZ_KEY.equals(name))) {
				if (AttributeType.association.equals(type)) {
					column = new BizPortColumn(attribute.getLocalisedDisplayName() + " ID", attribute.getLocalisedDescription(), type);
					Association association = (Association) attribute;
					Module owningModule = customer.getModule(currentDocument.getOwningModuleName());
					Document associationDocument = owningModule.getDocument(customer, association.getDocumentName());
					column.setReferencedSheet(new SheetKey(associationDocument.getOwningModuleName(), 
															associationDocument.getName()));
				}
				else {
					column = new BizPortColumn(attribute.getLocalisedDisplayName(), attribute.getLocalisedDescription(), type);
				}

				if (DomainType.constant.equals(attribute.getDomainType())) {
					if (bizlet == null) {
						bizlet = currentDocument.getBizlet(customer);
					}
					if (bizlet == null) { // metadata has an error
						bizlet = new Bizlet<>() {
							private static final long serialVersionUID = 5302965331084582623L;
						};
					}
					column.setRangeValues(getConstantRangeValues(bizlet, 
																	currentDocument.getOwningModuleName(),
																	currentDocument.getName(),
																	attribute));
				}

				sheet.addColumn(name, column);
			}
		}

		// add the parent if this is a child document
		String parentDocumentName = currentDocument.getParentDocumentName();
		if (parentDocumentName != null) {
			if (parentDocumentName.equals(document.getName())) {
				column = new BizPortColumn("Parent ID", "This points to the parent in a hiearachy within this shee", AttributeType.text);
			}
			else {
				Document parentDocument = currentDocument.getParentDocument(customer);
				String localisedSingularAlias = parentDocument.getSingularAlias();
				column = new BizPortColumn(localisedSingularAlias + " ID (Parent)",
												"The 'Parent' link of the relationship.  Populate this with " + localisedSingularAlias + " IDs.", 
												AttributeType.text);
				column.setReferencedSheet(new SheetKey(parentDocument.getOwningModuleName(), parentDocument.getName()));
				sheet.addColumn(ChildBean.PARENT_NAME, column);
	
				Reference reference = ((DocumentImpl) parentDocument).getReferenceByDocumentName(currentDocument.getName());
				if ((reference instanceof Collection) && Boolean.TRUE.equals(((Collection) reference).getOrdered())) {
					column = new BizPortColumn("Ordinal", "The order of these records", AttributeType.integer);
					sheet.addColumn(Bean.ORDINAL_NAME, column);
				}
			}
		}

		workbook.addSheet(new SheetKey(currentDocument.getOwningModuleName(), currentDocument.getName()), sheet);
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
	private void generateAndAddCollectionSheet(String binding,
												Document owningDocument,
												Collection collection,
												Document collectionDocument,
												BizPortWorkbook workbook) {
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
	private String[] getConstantRangeValues(Bizlet<?> domainBizlet,
												String domainModuleName,
												String domainDocumentName,
												Attribute domainAttribute) 
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
