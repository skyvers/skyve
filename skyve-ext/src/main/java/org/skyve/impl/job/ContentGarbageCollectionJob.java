package org.skyve.impl.job;

import java.util.Date;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResult;
import org.skyve.domain.Bean;
import org.skyve.impl.backup.ContentChecker;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.model.list.RDBMSDynamicPersistenceListModel;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.Thumbnail;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.annotation.Nullable;

/**
 * This job removes orphaned uploads and any textually indexed data left from delete/truncate SQL statements issued.
 * Once a day it also deletes stored thumbnails older than 90 days.
 * 
 * @author sandsm01
 */
public class ContentGarbageCollectionJob implements Job {
	private static final long CONTENT_GC_ELIGIBLE_AGE_MILLIS = UtilImpl.CONTENT_GC_ELIGIBLE_AGE_MINUTES * 60000L;
	private static final String EXECUTE_PROBLEM_MESSAGE = "ContentGarbageCollectionJob.execute() problem...";

	/**
	 * How many days old a stored thumbnail has to be for the daily sweep to delete it.
	 * This removes the thumbnails of images that are gone; one still in use is just made again.
	 */
	private static final int THUMBNAIL_RETENTION_DAYS = 90;

	/**
	 * How long to wait between thumbnail sweeps - a day, in milliseconds.
	 */
	private static final long THUMBNAIL_SWEEP_INTERVAL_MILLIS = TimeUnit.DAYS.toMillis(1);

	/**
	 * When the thumbnails were last swept, as this job runs more often than the sweep needs to.
	 */
	private static final AtomicLong LAST_THUMBNAIL_SWEEP_MILLIS = new AtomicLong();

    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(ContentGarbageCollectionJob.class);
    private static final Logger CONTENT_LOGGER = Category.CONTENT.logger();

	private Set<String> orphanedAttachmentContentIds = new TreeSet<>();
	private Set<String> orphanedBeanBizIds = new TreeSet<>();
	
	@Override
	@SuppressWarnings({"java:S3776", "java:S6541"}) // complexity OK
	public void execute(@Nullable JobExecutionContext context)
	throws JobExecutionException {
		LOGGER.info("Start Content Garbage Collection");
		try {
			ContentChecker contentChecker = new ContentChecker();
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
							if (UtilImpl.CONTENT_TRACE) CONTENT_LOGGER.trace("ContentGarbageCollectionJob: FOUND customer={} : module={} : document={} : bizId={} : attribute={} : contentId={} : lastModified={}", 
																				customerName, moduleName, documentName, bizId, attributeName, contentId, lastModified);
							// only process this if its at least a day old.
							// Besides cutting out busy work on a data set in flux, it'll make sure that anyones freshly uploaded
							// content that hasn't been saved (not pointed to yet in the database) won't be removed.
							if (lastModified == null) {
								if (result.isAttachment()) {
									LOGGER.warn("ContentGarbageCollectionJob: Cannot determine whether to remove attachment content with bizId/contentId {}/{}", bizId, contentId);
								}
								else {
									LOGGER.warn("ContentGarbageCollectionJob: Cannot determine whether to remove bean content with bizId {}", bizId);
								}
							}
							else if ((System.currentTimeMillis() - lastModified.getTime()) > CONTENT_GC_ELIGIBLE_AGE_MILLIS) { // of eligible age
								ProvidedRepository r = ProvidedRepositoryFactory.getNullSafe();
								Customer customer = r.getCustomer(customerName);
								Module module = r.getModule(customer, moduleName);
								Document document = module.getDocument(customer, documentName);
								Persistent persistent = document.getPersistent();
								if (persistent == null) { // was persistent with content but now transient
									if (result.isAttachment()) {
										LOGGER.warn("ContentGarbageCollectionJob: Cannot determine whether to remove attachment content with bizId/contentId {}/{} as the owning document {}.{} is not persistent", bizId, contentId, moduleName, documentName);
									}
									else {
										LOGGER.warn("ContentGarbageCollectionJob: Cannot determine whether to remove bean content with bizId {} as the owning document {}.{} is not persistent", bizId, moduleName, documentName);
									}
									continue;
								}
								String persistentIdentifier = persistent.getPersistentIdentifier();
								if (persistentIdentifier == null) { // was persistent with content but now transient
									if (result.isAttachment()) {
										LOGGER.warn("ContentGarbageCollectionJob: Cannot determine whether to remove attachment content with bizId/contentId {}/{} as the owning document {}.{} is not directly persistent", bizId, contentId, moduleName, documentName);
									}
									else {
										LOGGER.warn("ContentGarbageCollectionJob: Cannot determine whether to remove bean content with bizId {} as the owning document {}.{} is not directly persistent", bizId, moduleName, documentName);
									}
									continue;
								}
								
								SQL query = null;
								StringBuilder sql = new StringBuilder(128);
								// Determine if dynamic - dynamic document or dynamic attribute
								boolean dynamic = document.isDynamic();
								if (! dynamic) {
									Attribute attribute = (attributeName == null) ? null : document.getPolymorphicAttribute(customer, attributeName);
									dynamic = (attribute != null) && BindUtil.isDynamic(customer, module, document, attribute);
								}

								// If dynamic look in ADM_DynamicEntity, otherwise look in the persistent identifier
								if (dynamic) {
									persistentIdentifier = RDBMSDynamicPersistenceListModel.getDynamicEntityPersistent(customer).getPersistentIdentifier();
								}

								sql.append("select 1 from ").append(persistentIdentifier);
								sql.append(" where ").append(Bean.DOCUMENT_ID).append(" = :").append(Bean.DOCUMENT_ID);
								
								// check if we have a record
								if (result.isAttachment()) { // attachment
									// If dynamic, look for the "attributeName":"contentId" combo in the fields JSON
									if (dynamic) {
										sql.append(" and fields like :").append(attributeName);
										
										query = p.newSQL(sql.toString());
										query.putParameter(Bean.DOCUMENT_ID, bizId, false);
										StringBuilder like = new StringBuilder(96);
										like.append("%\"").append(attributeName).append("\":\"").append(contentId).append("\"%");
										query.putParameter(attributeName, like.toString(), false);
									}
									else {
										// Look in the table column for the contentId
										sql.append(" and ").append(attributeName).append(" = :").append(attributeName);
			
										query = p.newSQL(sql.toString());
										query.putParameter(Bean.DOCUMENT_ID, bizId, false);
										query.putParameter(attributeName, contentId, false);
									}
								}
								else { // bean
									query = p.newSQL(sql.toString());
									query.putParameter(Bean.DOCUMENT_ID, bizId, false);
								}
								
								if (UtilImpl.CONTENT_TRACE) CONTENT_LOGGER.trace("ContentGarbageCollectionJob: TEST REMOVAL with {}", sql);
								if (query.scalarResults(Integer.class).isEmpty()) {
									if (result.isAttachment()) {
										String bogusContentReference = contentChecker.bogusContentReference(contentId, customer);
										if (bogusContentReference == null) {
											orphanedAttachmentContentIds.add(contentId);
											LOGGER.info("ContentGarbageCollectionJob: Remove attachment content with bizid/contentId {}/{}", bizId, contentId);
										}
										else {
											LOGGER.error("ContentGarbageCollectionJob: Cannot remove unreferenced attachment content with bizId/contentId {}/{} and owning document of {}.{} as it is actually referenced by Table#BizId {}", bizId, contentId, moduleName, documentName, bogusContentReference);
										}
									}
									else {
										orphanedBeanBizIds.add(bizId);
										LOGGER.info("ContentGarbageCollectionJob: Remove bean content with bizId {}", bizId);
									}
								}
							}
						}
						catch (Exception e) {
							LOGGER.warn("ContentGarbageCollectionJob retrieve problem...{}", e.getLocalizedMessage());
							if (UtilImpl.CONTENT_TRACE) CONTENT_LOGGER.warn(EXECUTE_PROBLEM_MESSAGE, e);
						}
					}
					
					for (String contentId : orphanedAttachmentContentIds) {
						try { // don't stop trying to remove content
							cm.removeAttachment(contentId);
						}
						catch (Exception e) {
							LOGGER.warn("ContentGarbageCollectionJob remove problem...{}", e.getLocalizedMessage());
							if (UtilImpl.CONTENT_TRACE) CONTENT_LOGGER.warn(EXECUTE_PROBLEM_MESSAGE, e);
						}
					}
					orphanedAttachmentContentIds.clear();

					for (String bizId : orphanedBeanBizIds) {
						try { // don't stop trying to remove content
							cm.removeBean(bizId);
						}
						catch (Exception e) {
							LOGGER.warn("ContentGarbageCollectionJob remove problem...{}", e.getLocalizedMessage());
							if (UtilImpl.CONTENT_TRACE) CONTENT_LOGGER.warn(EXECUTE_PROBLEM_MESSAGE, e);
						}
					}
					orphanedBeanBizIds.clear();
				}
			}
			finally {
				p.commit(true);
			}
			LOGGER.info("Successfully performed Content Garbage Collection");
			sweepOldThumbnails(System.currentTimeMillis());
		}
		catch (Exception e) {
			throw new JobExecutionException("Error encountered whilst performing CMS garbage collection", e);
		}
	}

	/**
	 * Deletes stored thumbnails older than {@link #THUMBNAIL_RETENTION_DAYS}, if file storage is on
	 * and they haven't been swept in the last day. A problem is logged rather than thrown.
	 *
	 * @param now	The time now, in milliseconds.
	 */
	static void sweepOldThumbnails(long now) {
		if (! UtilImpl.THUMBNAIL_FILE_STORAGE) {
			return;
		}
		long last = LAST_THUMBNAIL_SWEEP_MILLIS.get();
		if (((last != 0L) && ((now - last) < THUMBNAIL_SWEEP_INTERVAL_MILLIS)) ||
				(! LAST_THUMBNAIL_SWEEP_MILLIS.compareAndSet(last, now))) {
			return;
		}
		try {
			int deleted = Thumbnail.deleteStoredThumbnailsOlderThan(THUMBNAIL_RETENTION_DAYS);
			LOGGER.info("ContentGarbageCollectionJob deleted {} stored thumbnail files older than {} days",
							Integer.valueOf(deleted), Integer.valueOf(THUMBNAIL_RETENTION_DAYS));
		}
		catch (Exception e) {
			LOGGER.warn("ContentGarbageCollectionJob could not delete old stored thumbnails - {}", e.getLocalizedMessage());
		}
	}
}
