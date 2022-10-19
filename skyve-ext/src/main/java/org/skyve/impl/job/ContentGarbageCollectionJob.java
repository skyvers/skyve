package org.skyve.impl.job;

import java.util.Date;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResult;
import org.skyve.domain.Bean;
import org.skyve.impl.backup.ContentChecker;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.Repository;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.Util;

/**
 * This job removes orphaned uploads and any textually indexed data left from delete/truncate SQL statements issued.
 * 
 * @author sandsm01
 */
public class ContentGarbageCollectionJob implements Job {
	private static final long CONTENT_GC_ELIGIBLE_AGE_MILLIS = UtilImpl.CONTENT_GC_ELIGIBLE_AGE_MINUTES * 60000L;
	
	private Set<String> orphanedAttachmentContentIds = new TreeSet<>();
	private Set<String> orphanedBeanBizIds = new TreeSet<>();
	
	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		Util.LOGGER.info("Start Content Garbage Collection");
		try {
			ContentChecker contentChecker = new ContentChecker();
			Repository r = CORE.getRepository();
			Persistence p = CORE.getPersistence();
			try {
				try (ContentManager cm = EXT.newContentManager()) {
					for (SearchResult result : cm.all()) {
						// Protect against unbounded GC operations
						if (result.isAttachment()) {
							if (orphanedAttachmentContentIds.size() > 10000) {
								continue; // can't break here as we need to ensure the iterable is closed
							}
						}
						else {
							if (orphanedBeanBizIds.size() > 10000) {
								continue; // can't break here as we need to ensure the iterable is closed
							}
						}
						
						try { // don't stop trying to detect removed content
							String customerName = result.getCustomerName();
							String moduleName = result.getModuleName();
							String documentName = result.getDocumentName();
							String attributeName = result.getAttributeName();
							String bizId = result.getBizId();
							String contentId = result.getContentId();
							Date lastModified = result.getLastModified();
							if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.finest("ContentGarbageCollectionJob: FOUND customer=" + customerName + 
																				" : module=" + moduleName + 
																				" : document=" + documentName + 
																				" : bizId=" + bizId + 
																				" : attribute=" + attributeName + 
																				" : contentId=" + contentId + 
																				" : lastModified=" + lastModified);
							// only process this if its at least a day old.
							// Besides cutting out busy work on a data set in flux, it'll make sure that anyones freshly uploaded
							// content that hasn't been saved (not pointed to yet in the database) won't be removed.
							if (lastModified == null) {
								if (result.isAttachment()) {
									UtilImpl.LOGGER.warning("ContentGarbageCollectionJob: Cannot determine whether to remove attachment content with bizId/contentId " + bizId + "/" + contentId);
								}
								else {
									UtilImpl.LOGGER.warning("ContentGarbageCollectionJob: Cannot determine whether to remove bean content with bizId " + bizId);
								}
							}
							else if ((System.currentTimeMillis() - lastModified.getTime()) > CONTENT_GC_ELIGIBLE_AGE_MILLIS) { // of eligible age
								Customer customer = r.getCustomer(customerName);
								Module module = customer.getModule(moduleName);
								Document document = module.getDocument(customer, documentName);
								Persistent persistent = document.getPersistent();
								if (persistent == null) { // was persistent with content but now transient
									if (result.isAttachment()) {
										UtilImpl.LOGGER.warning("ContentGarbageCollectionJob: Cannot determine whether to remove attachment content with bizId/contentId " + bizId + "/" + contentId + " as the owning document " + moduleName + "." + documentName + " is not persistent");
									}
									else {
										UtilImpl.LOGGER.warning("ContentGarbageCollectionJob: Cannot determine whether to remove bean content with bizId " + bizId + " as the owning document " + moduleName + "." + documentName + " is not persistent");
									}
									continue;
								}
								String persistentIdentifier = persistent.getPersistentIdentifier();
								if (persistentIdentifier == null) { // was persistent with content but now transient
									if (result.isAttachment()) {
										UtilImpl.LOGGER.warning("ContentGarbageCollectionJob: Cannot determine whether to remove attachment content with bizId/contentId " + bizId + "/" + contentId + " as the owning document " + moduleName + "." + documentName + " is not directly persistent");
									}
									else {
										UtilImpl.LOGGER.warning("ContentGarbageCollectionJob: Cannot determine whether to remove bean content with bizId " + bizId + " as the owning document " + moduleName + "." + documentName + " is not directly persistent");
									}
									continue;
								}
								
								SQL query = null;
								StringBuilder sql = new StringBuilder(128);
								sql.append("select 1 from ").append(persistentIdentifier);
								sql.append(" where ").append(Bean.DOCUMENT_ID).append(" = :").append(Bean.DOCUMENT_ID);
								
								// check if we have a record
								if (result.isAttachment()) { // attachment
									sql.append(" and ").append(attributeName).append(" = :").append(attributeName);
		
									query = p.newSQL(sql.toString());
									query.putParameter(Bean.DOCUMENT_ID, bizId, false);
									query.putParameter(attributeName, contentId, false);
								}
								else { // bean
									query = p.newSQL(sql.toString());
									query.putParameter(Bean.DOCUMENT_ID, bizId, false);
								}
								
								if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.finest("ContentGarbageCollectionJob: TEST REMOVAL with " + sql.toString());
								if (query.scalarResults(Integer.class).isEmpty()) {
									if (result.isAttachment()) {
										String bogusContentReference = contentChecker.bogusContentReference(contentId);
										if (bogusContentReference == null) {
											orphanedAttachmentContentIds.add(contentId);
											UtilImpl.LOGGER.info("ContentGarbageCollectionJob: Remove attachment content with bizid/contentId " + contentId + "/" + bizId);
										}
										else {
											UtilImpl.LOGGER.severe("ContentGarbageCollectionJob: Cannot remove unreferenced attachment content with bizId/contentId " + contentId + "/" + bizId + " and owning document of " + moduleName + "." + documentName + " as it is actually referenced by Table#BizId " + bogusContentReference);
										}
									}
									else {
										orphanedBeanBizIds.add(bizId);
										UtilImpl.LOGGER.info("ContentGarbageCollectionJob: Remove bean content with bizId " + bizId);
									}
								}
							}
						}
						catch (Exception e) {
							Util.LOGGER.warning("ContentGarbageCollectionJob retrieve problem..." + e.getLocalizedMessage());
							if (UtilImpl.CONTENT_TRACE) Util.LOGGER.log(Level.WARNING, "ContentGarbageCollectionJob.execute() problem...", e);
						}
					}
					
					for (String contentId : orphanedAttachmentContentIds) {
						try { // don't stop trying to remove content
							cm.removeAttachment(contentId);
						}
						catch (Exception e) {
							Util.LOGGER.warning("ContentGarbageCollectionJob remove problem..." + e.getLocalizedMessage());
							if (UtilImpl.CONTENT_TRACE) Util.LOGGER.log(Level.WARNING, "ContentGarbageCollectionJob.execute() problem...", e);
						}
					}
					orphanedAttachmentContentIds.clear();

					for (String bizId : orphanedBeanBizIds) {
						try { // don't stop trying to remove content
							cm.removeBean(bizId);
						}
						catch (Exception e) {
							Util.LOGGER.warning("ContentGarbageCollectionJob remove problem..." + e.getLocalizedMessage());
							if (UtilImpl.CONTENT_TRACE) Util.LOGGER.log(Level.WARNING, "ContentGarbageCollectionJob.execute() problem...", e);
						}
					}
					orphanedBeanBizIds.clear();
				}
			}
			finally {
				p.commit(true);
			}
			Util.LOGGER.info("Successfully performed Content Garbage Collection");
		}
		catch (Exception e) {
			throw new JobExecutionException("Error encountered whilst performing CMS garbage collection", e);
		}
	}
}