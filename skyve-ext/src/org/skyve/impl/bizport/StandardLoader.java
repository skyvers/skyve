package org.skyve.impl.bizport;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.bizport.BizPortColumn;
import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.bizport.SheetKey;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.impl.bizport.StandardGenerator;

/**
 * This class assembles all the contain beans into a big map keyed by "module.document.bizId", 
 * links them together through their foreign key columns and collection sheets,
 * and makes them available to the caller for further processing.
 * 
 * The Excel worksheet must be in a format similar to a generated Excel worksheet - see BizportGenerator.
 * The first document sheet encountered during processing is considered the driving document.
 * 
 * The caller will typically construct the object, call populate() and then getBeans() etc.
 * 
 * @author mike
 */
public class StandardLoader {
	private BizPortWorkbook workbook;
	
	// The exception that will be thrown at the end of processing if any messages are present
	private BizPortException problems;
	
	// Map of sheet IDs in the spreadsheet by bizId
	// This map contains all the refs in the spreadsheet including child beans, child's child beans.
	// bizId -> sheetId
	private Map<String, Object> refs = new TreeMap<>();
	
	// Map of beans by Sheet key - model.document.excelId
	// This map contains all the beans in the spreadsheet including child beans, child's child beans.
	private Map<String, Bean> beansBySheetKey = new TreeMap<>();
	
	public StandardLoader(BizPortWorkbook workbook, BizPortException problems) {
		this.workbook = workbook;
		this.problems = problems;
	}

	/**
	 * Populate from document sheets.
	 * Link from association columns.
	 * Link from collection sheets.
	 * 
	 * @param <T>	The inferred type for a collection of beans from the driving document (first document sheet)
	 * @param persistence	The persistence to use during population.
	 * @return	A list of top-level beans from the first document sheet found.
	 * @throws Exception
	 */
	public <T extends Bean> List<T> populate(Persistence persistence)
	throws Exception {
		List<T> result = new ArrayList<>(128);
		
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		
		// Populate from document sheets
		String drivingDocumentName = null;
		for (SheetKey key : workbook.getSheetKeys()) {
			if (key.getCollectionBinding() == null) { // document sheet
				BizPortSheet sheet = workbook.getSheet(key);
				Module module = customer.getModule(key.getModuleName());
				Document document = module.getDocument(customer, key.getDocumentName());
				if (drivingDocumentName == null) {
					drivingDocumentName = key.getDocumentName();
					populateBeansFromSheet(persistence, user, document, sheet, result);
				}
				else {
					populateBeansFromSheet(persistence, user, document, sheet, null);
				}
			}
		}
		
		// Populate from association columns within documents
		for (SheetKey key : workbook.getSheetKeys()) {
			if (key.getCollectionBinding() == null) { // document sheet
				BizPortSheet sheet = workbook.getSheet(key);
				Module module = customer.getModule(key.getModuleName());
				Document document = module.getDocument(customer, key.getDocumentName());
				linkAssociationsFromSheet(customer, module, document, sheet);
			}
		}

		// Populate from collection sheets
		for (SheetKey key : workbook.getSheetKeys()) {
			String collectionBinding = key.getCollectionBinding(); 
			if (collectionBinding != null) { // relationship sheet
				Module module = customer.getModule(key.getModuleName());
				Document owningDocument = module.getDocument(customer, key.getDocumentName());
				Collection collection = (Collection) owningDocument.getReferenceByName(collectionBinding);
				Document elementDocument = module.getDocument(customer, collection.getDocumentName());
				BizPortSheet sheet = workbook.getSheet(key);
				populateCollectionFromSheet(collectionBinding,
												owningDocument, 
												elementDocument, 
												sheet);
			}
		}

		return result;
	}
	
	/**
	 * Runs through each defined row in the sheet given, creates beans conforming to
	 * the document parameter and appends them to the listToAddBeanTo parameter.
	 * 
	 * @param <T>The inferred type for the bean using the document parameter.
	 * @param persistence	The persistence instance to use during bean population.
	 * @param user	The logged in user.
	 * @param document	The document of the bean to create.
	 * @param sheet	The Excel sheet we are populating from.
	 * @param listToAddBeanTo	The list to add the constructed bean to.
	 * @throws Exception
	 */
	private <T extends Bean> void populateBeansFromSheet(Persistence persistence,
															User user,
															Document document,
															BizPortSheet sheet, 
															List<T> listToAddBeanTo)
	throws Exception {
		while (sheet.nextRow()) {
			T bean = populateBeanFromRow(persistence, user, document, sheet);
			Object sheetId = sheet.getValue(Bean.DOCUMENT_ID, AttributeType.text, problems);
			if (sheetId != null) {
				beansBySheetKey.put(createSheetKey(document, sheetId), bean);
				refs.put(bean.getBizId(), sheetId);

				if (listToAddBeanTo != null) {
					listToAddBeanTo.add(bean);
				}
			}
		}
		sheet.resetRow();
	}
	
	/**
	 * 
	 * @param <T>The inferred type for the bean using the document parameter.
	 * @param persistence	The persistence instance to use during bean population.
	 * @param user	The logged in user.
	 * @param document	The document of the bean to create.
	 * @param sheet	The Excel sheet we are populating from.
	 * @param listToAddBeanTo	The list to add the constructed bean to.
	 * @return	The constructed bean.
	 * @throws Exception
	 */
	private <T extends Bean> T populateBeanFromRow(Persistence persistence, 
													User user,
													Document document,
													BizPortSheet sheet) 
	throws Exception {
		T result = getBeanForRow(persistence, user, document, sheet);

		for (Attribute attribute : document.getAllAttributes()) {
			if (! (attribute instanceof Relation)) {
				String binding = attribute.getName();
				// Ignore bizId, owner id and element id, bizKey columns
				if (Bean.DOCUMENT_ID.equals(binding) ||
						StandardGenerator.OWNER_ID.equals(binding) ||
						StandardGenerator.ELEMENT_ID.equals(binding) ||
						Bean.BIZ_KEY.equals(binding)) {
					continue;
				}

				BizPortColumn column = sheet.getColumn(binding);
				if (column != null) { // column exists
					BindUtil.convertAndSet(result, 
											binding, 
											sheet.getValue(binding, 
															attribute.getAttributeType(), 
															problems));
				}
				else {
					sheet.addWarningAtCurrentRow(problems, 
													column, 
													"Column with binding " + binding + " does not exist in the " + sheet.getTitle() + " sheet. Check the template you started with and copy the missing column back in.");
				}
			}
		}

		return result;
	}
	
	/**
	 * Override this to get the bean for a document sheet row.
	 * By default this method looks for a bizId value in the 
	 * ID column and loads up the bean based on the sheet's module and document names
	 * or creates a new one if it is not found in the data store.
	 * 
	 * @param <T>	Anything that extends Bean
	 * @param persistence	The persistence to use, if needed
	 * @param user	The currently logged in user
	 * @param document	The document of the bean
	 * @param sheet	The sheet we are processing
	 * @return	The bean.
	 * @throws Exception
	 */
	protected <T extends Bean> T getBeanForRow(Persistence persistence, 
												User user,
												Document document,
												BizPortSheet sheet)
	throws Exception {
		T result = null;
		
		// Find the bizId column in the sheet
		BizPortColumn idColumn = sheet.getColumn(Bean.DOCUMENT_ID);
		String id = null;
		if (idColumn == null) { // no ID column
			result = document.newInstance(user);
		}
		else {
			id = sheet.getValue(Bean.DOCUMENT_ID, AttributeType.text, problems);
			// find the bean by bizId
			try {
				result = persistence.retrieve(document, id, true);
			} 
			catch (Exception e) { // could not be retrieved, must be new
				result = document.newInstance(user);
			}
		}
		
		return result;
	}

	/**
	 * The key is "module.document.sheetId" which is always unique.
	 * The customer is implied in the key and enforced by the persistence operations.
	 * 
	 * @param document	The document to create the key for.
	 * @param sheetId	The sheetId to create the bean for, (retrieved from the spreadsheet as an Object).
	 * @return	The key for the sheet row.
	 */
	public static String createSheetKey(Document document, Object sheetId) {
		StringBuilder result = new StringBuilder(64);
		result.append(document.getOwningModuleName()).append('.');
		result.append(document.getName()).append('.').append(sheetId);
		
		return result.toString();
	}
	
	/**
	 * Link/Populate all the beans together based on association columns present.
	 * 
	 * @param customer	The pertinent customer.
	 * @param document	The parent document to interrogate.
	 * @param sheet	The sheet to iterate over.
	 * @throws Exception
	 */
	private void linkAssociationsFromSheet(Customer customer, 
											Module module, 
											Document document, 
											BizPortSheet sheet)
	throws Exception {
		List<Association> associations = new ArrayList<>();
		
		// determine the association bindings
		for (String binding : sheet.getColumnBindings()) {
			Reference reference = document.getReferenceByName(binding);
			if (reference instanceof Association) {
				associations.add((Association) reference);
			}
		}
		
		// Check for the implicit parent association also
		String parentDocumentName = document.getParentDocumentName();
		if ((parentDocumentName != null) || (! associations.isEmpty())) {
			while (sheet.nextRow()) {
				// if this is a child document, link up this bean to its parent
				if (parentDocumentName != null) {
					linkParentFromRow(sheet, customer, document);
				}
				// process the associations (FKs)
				else {
					Object masterSheetId = sheet.getValue(Bean.DOCUMENT_ID, AttributeType.text, problems);
					if (masterSheetId != null) {
						linkAssociationsFromRow(sheet,
													customer,
													module,
													beansBySheetKey.get(createSheetKey(document, masterSheetId)),
													associations);
					}
				}
			}
		}
	}
	
	/**
	 * Link/Populate the child and parent bean together given the parent bizId.
	 * 
	 * @param sheet	To get values from the current row.
	 * @param customer	The pertinent customer.
	 * @param childDocument	The document of the child end of the relationship.
	 * @throws Exception
	 */
	private void linkParentFromRow(BizPortSheet sheet,
										Customer customer,
										Document childDocument)
	throws Exception {
		Object parentSheetId = sheet.getValue(ChildBean.PARENT_NAME, AttributeType.text, problems);
		Object childSheetId = sheet.getValue(Bean.DOCUMENT_ID, AttributeType.text, problems);
		if ((parentSheetId == null) && (childSheetId == null)) { // don't process an empty row
			return;
		}
		if (parentSheetId == null) {
			sheet.addErrorAtCurrentRow(problems, 
										sheet.getColumn(ChildBean.PARENT_NAME), 
										"No parent ID value is defined for the parent cell. You must define a parent ID value which matches the ID of a row in the parent sheet.");
			return;
		}
		if (childSheetId == null) {
			sheet.addErrorAtCurrentRow(problems, 
										sheet.getColumn(Bean.DOCUMENT_ID), 
										"No child ID value is defined for the parent cell. You must define a child ID value which matches the ID of a row in the child sheet.");
			return;
		}

		BizPortColumn parentKeyColumn = sheet.getColumn(ChildBean.PARENT_NAME);
		SheetKey parentKeySheetKey = parentKeyColumn.getReferencedSheet();
		BizPortSheet parentKeySheet = workbook.getSheet(parentKeySheetKey);
		if (parentKeySheet == null) {
			sheet.addWarningAtCurrentRow(problems, 
											parentKeyColumn, 
											"The parent sheet does not exist in this workbook. Check the template you started with and copy the missing sheet back in.");
			return;
		}

		Document parentDocument = childDocument.getParentDocument(customer);
		Reference childReference = ((DocumentImpl) parentDocument).getReferenceByDocumentName(childDocument.getName());
		String collectionBinding = childReference.getName();
		Bean parentBean = beansBySheetKey.get(createSheetKey(parentDocument, parentSheetId));
		if (parentBean == null) {
			sheet.addErrorAtCurrentRow(problems, 
										sheet.getColumn(ChildBean.PARENT_NAME), 
										"The parent ID " + parentSheetId + " does not match any row in the parent (target) sheet.");
			return;
		}
		@SuppressWarnings("unchecked")
		ChildBean<Bean> childBean = (ChildBean<Bean>) beansBySheetKey.get(createSheetKey(childDocument, childSheetId));
		if (childBean == null) {
			sheet.addErrorAtCurrentRow(problems, 
										sheet.getColumn(Bean.DOCUMENT_ID),
										"The child ID " + childSheetId + " does not match any row in the current sheet.");
			return;
		}

		BindUtil.ensureElementIsInCollection(parentBean, collectionBinding, childBean);
		childBean.setParent(parentBean);
	}
	
	/**
	 * Link/Populate all the associations present in the current row of the given sheet.
	 * 
	 * @param sheet	The sheet being processed.
	 * @param masterBean	The owning end of the relationship.
	 * @param associationBindings	The bindings to check and link/populate.
	 * @throws Exception
	 */
	private void linkAssociationsFromRow(BizPortSheet sheet, 
											Customer customer,
											Module module,
											Bean masterBean,
											List<Association> associations) 
	throws Exception {
		// for each association
		for (Association association : associations) {
			String associationBinding = association.getName();
			Object foreignKeySheetId = sheet.getValue(associationBinding, AttributeType.text, problems);
			if (foreignKeySheetId == null) {
				BindUtil.set(masterBean, associationBinding, null);
				continue;
			}
			
			// Process the FK ID - FK column is not null so the column definition must exist
			BizPortColumn foreignKeyColumn = sheet.getColumn(associationBinding);
			SheetKey foreignKeySheetKey = foreignKeyColumn.getReferencedSheet();
			// if the column has been redefined to not be a foreign key column,
			// then we do not try to resolve the reference
			if (foreignKeySheetKey == null) {
				continue;
			}
			BizPortSheet foreignKeySheet = workbook.getSheet(foreignKeySheetKey);
			if (foreignKeySheet == null) {
				sheet.addWarningAtCurrentRow(problems, 
												foreignKeyColumn, 
												"The referenced sheet for " + associationBinding + " does not exist in this workbook. Check the template you started with and copy the missing sheet back in.");
				continue;
			}

			Document associatedDocument = module.getDocument(customer, association.getDocumentName());
			Bean foreignKeyBean = beansBySheetKey.get(createSheetKey(associatedDocument, foreignKeySheetId));
			if (foreignKeyBean == null) {
				sheet.addErrorAtCurrentRow(problems, 
											foreignKeyColumn, 
											"The row referenced for " + associationBinding + " does not exist in the " + foreignKeySheet.getTitle() + " sheet. Check that the ID you have entered matches the ID of the row in the " + foreignKeySheet.getTitle() + " sheet.");
				continue;
			}

			BindUtil.set(masterBean, associationBinding, foreignKeyBean);
		}
	}

	/**
	 * Link/Populate collections from the joiner collection sheets.
	 * 
	 * @param collectionBinding	The collection binding to process.
	 * @param collectionSheet	The collection sheet to iterate over.
	 * @throws Exception
	 */
	private void populateCollectionFromSheet(String collectionBinding,
												Document owningDocument,
												Document elementDocument,
												BizPortSheet collectionSheet)
	throws Exception {
		while (collectionSheet.nextRow()) {
			populateCollectionFromRow(collectionBinding,
										owningDocument,
										elementDocument,
										collectionSheet);
		}
	}
	
	/**
	 * Link/populate a collection from a row in a collection sheet.
	 * 
	 * @param collectionBinding	The binding of the collection relative to the master document.
	 * @param collectionSheet	The collection sheet to get the current row values from.
	 * @throws Exception
	 */
	private void populateCollectionFromRow(String collectionBinding,
											Document owningDocument,
											Document elementDocument,
											BizPortSheet collectionSheet) 
	throws Exception {
		Object ownerSheetId = collectionSheet.getValue(StandardGenerator.OWNER_ID, AttributeType.text, problems);
		BizPortColumn ownerSheetIdColumn = collectionSheet.getColumn(StandardGenerator.OWNER_ID);
		if (ownerSheetId == null) {
			collectionSheet.addErrorAtCurrentRow(problems, 
													ownerSheetIdColumn, 
													"The owner cell is empty. You must specify the owner of this relationship.");
			return;
		}
		Object elementSheetId = collectionSheet.getValue(StandardGenerator.ELEMENT_ID, AttributeType.text, problems);
		BizPortColumn elementSheetIdColumn = collectionSheet.getColumn(StandardGenerator.ELEMENT_ID);
		if (elementSheetId == null) {
			collectionSheet.addErrorAtCurrentRow(problems, 
													elementSheetIdColumn, 
													"The element cell is empty. You must specify the element of this relationship.");
			return;
		}
		
		// Lookup the foreign key row for owner
		SheetKey ownerSheetKey = ownerSheetIdColumn.getReferencedSheet();
		BizPortSheet ownerSheet = workbook.getSheet(ownerSheetKey);
		if (ownerSheet == null) {
			collectionSheet.addErrorAtCurrentRow(problems, 
													ownerSheetIdColumn, 
													"The referenced sheet for " + StandardGenerator.OWNER_ID + " does not exist in this workbook. Check the template you started with and copy the missing sheet back in.");
			return;
		}
		Bean owner = beansBySheetKey.get(createSheetKey(owningDocument, ownerSheetId));
		if (owner == null) {
			collectionSheet.addErrorAtCurrentRow(problems, 
													ownerSheetIdColumn, 
													"The row for the collection owner does not exist in the " + ownerSheet.getTitle() + " sheet. Check that the ID you have entered matches the ID in the row in the " + ownerSheet.getTitle() + " sheet.");
			return;
		}

		// Lookup the foreign key row for element
		SheetKey elementSheetKey = elementSheetIdColumn.getReferencedSheet();
		BizPortSheet elementSheet = workbook.getSheet(elementSheetKey);
		if (elementSheet == null) {
			collectionSheet.addWarningAtCurrentRow(problems, 
													elementSheetIdColumn, 
													"The referenced sheet for " + StandardGenerator.ELEMENT_ID + " does not exist in this workbook. Check the template you started with and copy the missing sheet back in.");
			return;
		}
		Bean element = beansBySheetKey.get(createSheetKey(elementDocument, elementSheetId));
		if (element == null) {
			collectionSheet.addErrorAtCurrentRow(problems, 
													elementSheetIdColumn, 
													"The row for the collection element does not exist in the " + elementSheet.getTitle() + " sheet. Check that the ID you have entered matches the ID in the row in the " + elementSheet.getTitle() + " sheet.");
			return;
		}

		String referenceBinding = collectionBinding;
		int lastDotIndex = referenceBinding.lastIndexOf('.');
		if (lastDotIndex >= 0) {
			referenceBinding = referenceBinding.substring(lastDotIndex + 1);
		}
		BindUtil.ensureElementIsInCollection(owner, referenceBinding, element);
	}
	
	/**
	 * Get all keys ("module.document.sheetId") collected during populate().
	 * @return	An Iterable of all bizkeys found.
	 */
	public Iterable<String> getBeanKeys() {
		return beansBySheetKey.keySet();
	}
	
	/**
	 * Get a bean for a given key ("module.document.sheetId").
	 * @param key	The key.
	 * @return	The bean associated with the given key, or null.
	 */
	public Bean getBean(String key) {
		return beansBySheetKey.get(key);
	}
	
	/**
	 * Get the key into a sheet row given a bizId.
	 * 
	 * @param bizId	The bizId
	 * @return	The sheetId
	 */
	public Object getSheetKeyFromBizId(String bizId) {
		return refs.get(bizId);
	}
	
	/**
	 * Map a bean under the given key (should be "module.document.sheetId" by default).
	 * 
	 * @param key	The key to place the bean under.
	 * @param bean	The bean to map.
	 */
	public void putBean(String key, Bean bean) {
		beansBySheetKey.put(key, bean);
	}
	
	public void addError(Customer customer, Bean bean, DomainException e) {
		BizPortSheet sheet = workbook.getSheet(new SheetKey(bean.getBizModule(), bean.getBizDocument()));
		BizPortColumn column = sheet.getColumn(Bean.DOCUMENT_ID);
		Object sheetId = refs.get(bean.getBizId());
		sheet.moveToRow(sheetId);
		if (e instanceof MessageException) {
			Module module = customer.getModule(bean.getBizModule());
			Document document = module.getDocument(customer, bean.getBizDocument());
			for (Message em : ((MessageException) e).getMessages()) {
				addError(customer, module, document, bean, sheet, em);
			}
		}
		else {
			sheet.addErrorAtCurrentRow(problems, column, e.getMessage());
		}
	}
	
	// Ensure that error message and all subordinate error messages fit on the message list
	private void addError(Customer customer, 
							Module module, 
							Document document, 
							Bean bean,
							BizPortSheet sheet, 
							Message em) {
		boolean noBindings = true;
		for (String binding : em.getBindings()) {
			if (binding.indexOf('.') > 0) { // compound binding
				try {
					TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
					Document targetDocument = target.getDocument();
					BizPortSheet targetSheet = workbook.getSheet(new SheetKey(targetDocument.getOwningModuleName(),
																				targetDocument.getName()));
					if (targetSheet != null) {
						Attribute targetAttribute = target.getAttribute();
						BizPortColumn column = null;
						if (targetAttribute != null) {
							column = targetSheet.getColumn(targetAttribute.getName());
						}
						if (column == null) {
							column = targetSheet.getColumn(Bean.DOCUMENT_ID);
						}
						Bean targetBean = (Bean) BindUtil.get(bean, binding.substring(0, binding.lastIndexOf('.')));
						if (targetBean != null) {
							Object sheetId = refs.get(targetBean.getBizId());
							if (sheetId != null) {
								if (targetSheet.moveToRow(sheetId)) {
									targetSheet.addErrorAtCurrentRow(problems, column, em.getErrorMessage());
									noBindings = false;
								}								
							}
						}
					}
				} catch (Exception e) {
					// do nothing here - we'll end up with a wrong "Problem.where" attribute down below anyway.
				}
			}
			else {
				BizPortColumn column = sheet.getColumn(binding);
				if (column == null) {
					column = sheet.getColumn(Bean.DOCUMENT_ID);
				}
				sheet.addErrorAtCurrentRow(problems, column, em.getErrorMessage());
				noBindings = false;
			}
		}
		
		if (noBindings) {
			BizPortColumn column = sheet.getColumn(Bean.DOCUMENT_ID);
			sheet.addErrorAtCurrentRow(problems, column, em.getErrorMessage());
		}
	}
}
