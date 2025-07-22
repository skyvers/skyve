package org.skyve.impl.backup;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.model.document.field.Memo;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.view.model.list.RDBMSDynamicPersistenceListModel;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.SQL;

public class ReindexBeansJob extends CancellableJob {
	@Override
	public void execute() throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		Customer customer = persistence.getUser().getCustomer();
		String dynamicEntityPersistenceIdentifier = RDBMSDynamicPersistenceListModel.getDynamicEntityPersistent(customer).getPersistentIdentifier();
		List<String> log = getLog();
		String trace;
		
		// truncate the bean content ready to reindex
		try (ContentManager cm = EXT.newContentManager()) {
			trace = "Truncate Beans";
			log.add(trace);
			LOGGER.info(trace);
			cm.truncateBeanIndexing(customer.getName());
		}
		
		// reindex
		List<Module> modules = customer.getModules();
		float i = 0f;
		float l = modules.size();
		for (Module module : modules) {
			i++;
			String moduleName = module.getName();

			Map<String, DocumentRef> refs = module.getDocumentRefs();
			for (Entry<String, DocumentRef> entry : refs.entrySet()) {
				String documentName = entry.getKey();
				DocumentRef ref = entry.getValue();
				// is the document defined in this module?
				if (moduleName.equals(ref.getOwningModuleName())) {
					Document document = module.getDocument(customer, documentName);
					if (needsIndexing(customer, document)) {
						try {
							persistence.begin();
							try (ContentManager cm = EXT.newContentManager()) {
								// Don't check if a document has indexable fields as we
								// may need to have nodes deleted
								// (i.e. a document field used to be indexed but now is not)
								trace = String.format("Reindex document %s.%s", moduleName, documentName);
								log.add(trace);
								LOGGER.info(trace);
								if (document.isDynamic()) {
									SQL query = persistence.newSQL(String.format("select %s from %s where %s = :%s and %s = :%s",
																					Bean.DOCUMENT_ID,
																					dynamicEntityPersistenceIdentifier,
																					AppConstants.MODULE_NAME_ATTRIBUTE_NAME,
																					AppConstants.MODULE_NAME_ATTRIBUTE_NAME,
																					AppConstants.DOCUMENT_NAME_ATTRIBUTE_NAME,
																					AppConstants.DOCUMENT_NAME_ATTRIBUTE_NAME));
									query.putParameter(AppConstants.MODULE_NAME_ATTRIBUTE_NAME, moduleName, false);
									query.putParameter(AppConstants.DOCUMENT_NAME_ATTRIBUTE_NAME, documentName, false);
									try (AutoClosingIterable<String> it = query.scalarIterable(String.class)) {
										for (String bizId : it) {
											PersistentBean bean = persistence.retrieve(document, bizId);
											if (bean != null) {
												persistence.reindex(bean);
											}
											// Evict anything inadvertently loaded and cached by the reindex operation above
											persistence.evictAllCached();
										}
									}
								}
								else {
									DocumentQuery query = persistence.newDocumentQuery(document);
									query.noTimeout();
									try (AutoClosingIterable<PersistentBean> it = query.beanIterable()) {
										for (PersistentBean bean : it) {
											persistence.reindex(bean);
											// Evict anything inadvertently loaded and cached by the reindex operation above
											persistence.evictAllCached();
										}
									}
								}
							}
						}
						finally {
							persistence.commit(false);
						}
					}
					else {
						trace = String.format("Skipping document %s.%s", document.getOwningModuleName(), document.getName());
						getLog().add(trace);
						LOGGER.info(trace);
					}
				}
			}
			setPercentComplete((int) (i / l * 100f));
		}
		trace = "Reindex beans complete";
		log.add(trace);
		LOGGER.info(trace);
		setPercentComplete(100);
	}
	
	private static boolean needsIndexing(Customer customer, Document document) {
		if (document.isPersistable()) {
			for (Attribute attribute : document.getAllAttributes(customer)) {
				if (attribute instanceof Field field) {
					IndexType index = field.getIndex();
					// text indexing is required on this attribute
					if (IndexType.both.equals(index) || IndexType.textual.equals(index)) {
						return true;
					}
					// text indexing defaults to on for Memo (and Markup)
					if ((index == null) && (attribute instanceof Memo)) {
						return true;
					}
				}
			}
		}
		return false;
	}
}
