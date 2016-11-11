package org.skyve.impl.util;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.CustomerImpl.ExportedReference;
import org.skyve.impl.util.CascadeDeleteBeanVisitor;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.SQL;
import org.skyve.impl.util.ExportedReferenceVisitor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Provide a depth-first traversal of references as defined in the skyve metadata.
 * More specifically, it visits all the following references which target a given bean.
 * 1) Composed association
 * 2) Aggregated association
 * 3) Composed Collection
 * 4) Aggregated Collection
 * 5) Hierarchical Parent
 * 6) Structural Parent
 * 
 * It does not visit child collections.
 * This class is primary used to de-reference a bean that's about to be deleted.
 * The Dereferencer convenience class does just this.
 */
public abstract class ExportedReferenceVisitor {
	public void visit(Bean bean)
	throws Exception {
		CustomerImpl c = (CustomerImpl) CORE.getUser().getCustomer();
		visit(c, c.getModule(bean.getBizModule()).getDocument(c, bean.getBizDocument()), bean);
	}
	
	public void visit(Document document, Bean bean)
	throws Exception {
		CustomerImpl c = (CustomerImpl) CORE.getUser().getCustomer();
		visit(c, document, bean);
	}

	private void visit(final CustomerImpl customer, Document document, Bean bean) 
	throws Exception {
		final Set<String> bizIdsVisited = new TreeSet<>();
		
		new CascadeDeleteBeanVisitor() {
			@Override
			@SuppressWarnings("synthetic-access")
			public void preDeleteProcessing(Document documentToCascade,
												Bean beanToCascade)
			throws Exception {
				Set<String> documentsVisited = new TreeSet<>();
				visitBean(customer, documentToCascade, beanToCascade.getBizId(), documentsVisited, bizIdsVisited);
			}
		}.visit(document, bean, customer);
	}
	
	private void visitBean(CustomerImpl customer, 
							Document document, 
							String bizId, 
							Set<String> documentsVisited,
							Set<String> bizIdsVisited)
	throws Exception {
		if (bizIdsVisited.contains(bizId)) {
			return;
		}
		bizIdsVisited.add(bizId);
		
		List<ExportedReference> refs = customer.getExportedReferences(document);
		if (refs != null) {
			for (ExportedReference ref : refs) {
				String referenceModuleName = ref.getModuleName();
				String referenceDocumentName = ref.getDocumentName();

				Module referenceModule = customer.getModule(referenceModuleName);
				Document referenceDocument = referenceModule.getDocument(customer, referenceDocumentName);
				Persistent persistent = document.getPersistent();
				if (persistent != null) {
					visitReference(customer, document, bizId, ref, referenceDocument);
				}
			}
		}

		documentsVisited.add(new StringBuilder(32).append(document.getOwningModuleName()).append('.').append(document.getName()).toString());

		// Process base document if present
		String baseDocumentName = customer.getBaseDocument(document);
		if ((baseDocumentName != null) && (! documentsVisited.contains(baseDocumentName))) {
			int dotIndex = baseDocumentName.indexOf('.');
			Module baseModule = customer.getModule(baseDocumentName.substring(0, dotIndex));
			Document baseDocument = baseModule.getDocument(customer, baseDocumentName.substring(dotIndex + 1));
			visitBean(customer, baseDocument, bizId, documentsVisited, bizIdsVisited);
		}

		// Process derived documents if present
		for (String derivedDocumentName : customer.getDerivedDocuments(document)) {
			if ((derivedDocumentName != null) && (! documentsVisited.contains(derivedDocumentName))) {
				int dotIndex = derivedDocumentName.indexOf('.');
				Module derivedModule = customer.getModule(derivedDocumentName.substring(0, dotIndex));
				Document derivedDocument = derivedModule.getDocument(customer, derivedDocumentName.substring(dotIndex + 1));
				visitBean(customer, derivedDocument, bizId, documentsVisited, bizIdsVisited);
			}
		}
		
		Document parentDocument = document.getParentDocument(customer);
		if (parentDocument != null) {
			acceptParent(document, bizId, parentDocument);
		}
	}

	private void visitReference(CustomerImpl customer,
									Document document,
									String bizId,
									ExportedReference ref,
									Document referenceDocument)
	throws Exception {
		if (ExtensionStrategy.mapped.equals(referenceDocument.getPersistent().getStrategy())) {
			// Find all implementations below the mapped and check these instead
			Set<Document> derivations = new HashSet<>();
			populateImmediateMapImplementingDerivations(customer, referenceDocument, derivations);
			for (Document derivation : derivations) {
				visitReference(customer, document, bizId, ref, derivation);
			}
		}
		else {
			acceptReference(document, bizId, ref, referenceDocument);
		}
	}

	private void populateImmediateMapImplementingDerivations(CustomerImpl customer,
																Document document,
																Set<Document> result) {
		for (String derivedDocumentName : customer.getDerivedDocuments(document)) {
			int dotIndex = derivedDocumentName.indexOf('.');
			Module derivedModule = customer.getModule(derivedDocumentName.substring(0, dotIndex));
			Document derivedDocument = derivedModule.getDocument(customer, derivedDocumentName.substring(dotIndex + 1));

			Persistent derivedPersistent = derivedDocument.getPersistent();
			if ((derivedPersistent != null) && (derivedPersistent.getName() != null)) {
				result.add(derivedDocument);
			}
			else {
				populateImmediateMapImplementingDerivations(customer, derivedDocument, result);
			}
		}
	}

	/**
	 * Process the reference.
	 * 
	 * @param document	The target document of this reference (being pointed at).
	 * @param bizId	The bizId of the reference target.
	 * @param exportedReference The reference pointing to this document instance (bean)
	 * @param referenceDocument	The document that has this reference in it (is pointing to)
	 * @throws Exception
	 */
	protected abstract void acceptReference(Document document,
												String bizId,
												ExportedReference exportedReference,
												Document referenceDocument)
	throws Exception;

	/**
	 * Process the parent reference.
	 * 
	 * @param document	The target of this parent reference (being pointed at)
	 * @param bizId	The bizId of the target parent.
	 * @param parentDocument	The document that has the parent reference in it (is pointing to)
	 * @throws Exception
	 */
	protected abstract void acceptParent(Document document,
											String bizId,
											Document parentDocument)
	throws Exception;
	
	public static final class Dereferencer extends ExportedReferenceVisitor {
		
		private static final Logger logger = LoggerFactory.getLogger(Dereferencer.class);
		
		// Replace any mandatory references to be nulled out with this value
		// module.Document -> bizId
		private Map<String, String> newBizIds = new TreeMap<>();
		
		public Dereferencer() {
			// nothing to do here
		}

		/**
		 * Add a default bizId that will be used on mandatory references that can't be nulled.
		 * 
		 * @param document	The document the newBizId is for.
		 * @param newBizId	The value to set instead of null.
		 * @return	The Dereferencer for method chaining.
		 */
		public Dereferencer putDefault(Document document, String newBizId) {
			return putDefault(document.getOwningModuleName(), document.getName(), newBizId);
		}
		
		/**
		 * Add a default bizId that will be used on mandatory references that can't be nulled.
		 * 
		 * @param moduleName	The module name the newBizId is for.
		 * @param documentName	The document name the newBizId is for.
		 * @param newBizId	The value to set instead of null.
		 * @return	The Dereferencer for method chaining.
		 */
		public Dereferencer putDefault(String moduleName, String documentName, String newBizId) {
			newBizIds.put(moduleName + '.' + documentName, newBizId);
			return this;
		}

		@Override
		protected void acceptReference(Document document,
										String bizId,
										ExportedReference exportedReference, 
										Document referenceDocument)
		throws Exception {
			// This thing processes aggregated and composed associations
			// It doesn't really matter if we unlink the object graph we are traversing
			// as hibernate just issues the SQL based on the object in memory.
			// ie a composed association is dereferenced in the method, but hibernate still
			// issues SQL to remove the composed association anyway.
			// Including compositions here ensures that any multiple composition relations
			// or composition outside of the object graph we are deleting is dealt with.
			ReferenceType referenceType = exportedReference.getType(); 

			String newBizId = newBizIds.get(referenceDocument.getOwningModuleName() + '.' + referenceDocument.getName());
			if (exportedReference.isCollection()) {
				if (! CollectionType.child.equals(referenceType)) {
					StringBuilder statement = new StringBuilder(64);
					if ((newBizId != null) && (exportedReference.isRequired())) {
						statement.append("update ");
						statement.append(referenceDocument.getPersistent().getPersistentIdentifier());
						statement.append('_').append(exportedReference.getReferenceFieldName());
						statement.append(" set owner_id = :newBizId where element_id = :").append(Bean.DOCUMENT_ID);
						if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(statement.toString());
						logger.debug(statement.toString());
						SQL sql = CORE.getPersistence().newSQL(statement.toString());
						sql.putParameter(Bean.DOCUMENT_ID, bizId, false);
						sql.putParameter("newBizId",newBizId, false);
						sql.execute();
					}
					else {
						statement.append("delete from ");
						statement.append(referenceDocument.getPersistent().getPersistentIdentifier());
						statement.append('_').append(exportedReference.getReferenceFieldName());
						statement.append(" where element_id = :").append(Bean.DOCUMENT_ID);
						if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(statement.toString());
						logger.debug(statement.toString());
						CORE.getPersistence().newSQL(statement.toString()).putParameter(Bean.DOCUMENT_ID, bizId, false).execute();
					}
				}
			}
			else {
				String referenceFieldName = exportedReference.getReferenceFieldName();
				StringBuilder statement = new StringBuilder(64);
				statement.append("update ").append(referenceDocument.getPersistent().getPersistentIdentifier());
				statement.append(" set ").append(referenceFieldName).append("_id = :newBizId");
				statement.append(" where ").append(referenceFieldName).append("_id = :").append(Bean.DOCUMENT_ID);
				if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(statement.toString());
				logger.debug(statement.toString());
				SQL sql = CORE.getPersistence().newSQL(statement.toString());
				sql.putParameter(Bean.DOCUMENT_ID, bizId, false);
				sql.putParameter("newBizId", exportedReference.isRequired() ? newBizId : null, false);
				sql.execute();
			}
		}

		@Override
		protected void acceptParent(Document document,
										String bizId,
										Document parentDocument)
		throws Exception {
			if (document.equals(parentDocument)) { // hierarchical
				String newBizId = newBizIds.get(parentDocument.getOwningModuleName() + '.' + parentDocument.getName());

				StringBuilder statement = new StringBuilder(64);
				statement.append("update ").append(document.getPersistent().getPersistentIdentifier());
				statement.append(" set ").append(HierarchicalBean.PARENT_ID).append(" = :newBizId");
				statement.append(" where ").append(HierarchicalBean.PARENT_ID).append(" = :").append(Bean.DOCUMENT_ID);
				if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(statement.toString());
				logger.debug(statement.toString());
				SQL sql = CORE.getPersistence().newSQL(statement.toString());
				sql.putParameter(Bean.DOCUMENT_ID, bizId, false);
				sql.putParameter("newBizId", newBizId, false);
				sql.execute();
			}
		}
	}
}
