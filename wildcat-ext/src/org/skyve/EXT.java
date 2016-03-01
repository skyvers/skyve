package org.skyve;

import java.io.OutputStream;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperPrint;

import org.apache.poi.ss.usermodel.Workbook;
import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Job;
import org.skyve.metadata.user.User;
import org.skyve.report.ReportFormat;
import org.skyve.wildcat.bizport.POISheet;
import org.skyve.wildcat.bizport.POIWorkbook;
import org.skyve.wildcat.bizport.StandardGenerator;
import org.skyve.wildcat.content.AbstractContentManager;
import org.skyve.wildcat.content.ContentManager;
import org.skyve.wildcat.job.JobDescription;
import org.skyve.wildcat.job.JobScheduler;
import org.skyve.wildcat.util.MailAttachment;
import org.skyve.wildcat.util.MailUtil;
import org.skyve.wildcat.util.ReportUtil;
import org.skyve.wildcat.util.TagUtil;
import org.skyve.wildcat.util.UtilImpl;

/**
 * The central factory for creating all objects required in wildcat ext.
 * See {@link org.skyve.CORE} for creating objects implemented in the wildcat core API.
 */
public class EXT {
	/**
	 * Disallow instantiation
	 */
	private EXT() {
		// no-op
	}
	
	/**
	 * New file factory method for creating a BizPort excel workbook.
	 * 
	 * @return The new workbook.
	 */
	public static BizPortWorkbook newBizPortWorkbook(boolean ooxmlFormat) {
		return new POIWorkbook(ooxmlFormat);
	}

	/**
	 * Existing file factory method for creating a BizPort excel workbook.
	 * 
	 * @param customer	The current customer (usually for the logged in user).
	 * @param workbook	The POI workbook representing an existing excel file.
	 * @param e    This exception is will contain any errors encountered during
	 *             population of the bizport workbook data structure.
	 *             This method will automatically throw <code>e</code> if any errors are added to it.
	 * @throws MetaDataException   When the metadata is not valid.
	 * @throws BizPortException    When data or excel workbook structural errors are encountered.
	 */
	public static BizPortWorkbook newBizPortWorkbook(Customer customer, Workbook workbook, BizPortException e)
	throws MetaDataException, BizPortException {
		return new POIWorkbook(customer, workbook, e);
	}
	
	/**
	 * BizPort workbook sheet factory method for creating a new sheet.
	 * Note that the sheet is not added to any workbook.
	 * See {@link org.skyve.bizport.BizPortWorkbook#addSheet} for this.
	 * 
	 * @param title    The title of the sheet, used to name the excel sheet tab.
	 */
	public static BizPortSheet newBizPortSheet(String title) {
		return new POISheet(title);
	}
	
	/**
	 * Creates a new standard generator based on a driving document.
	 * 
	 * @param customer	The relevant customer for the generation.
	 * @param document	The driving document for the generation.
	 *                      The top-level document which the bizport excel document will model.
	 * @param exclusions	Bindings (relative to driving document) to exclude from the generation.
     *                      Each binding will stop the recursive generation processing when it is satisfied.
	 */
	public static StandardGenerator newBizPortStandardGenerator(Customer customer,
																	Document document,
																	String... exclusions) {
		return new StandardGenerator(customer, document, exclusions);
	}
	
	/**
	 * Write an email to the output stream <code>out</code> in MIME RFC 822 format with multiple attachments.
	 * 
	 * Outlook can load this format.
	 * Note - "on behalf of"...
	 * If the sender email address differs from the wildcat configured email credentials then,
	 * depending on the email client receiving the email at the destination, the email sending address
	 * may display with an indication that it wasn't actually sent from the email account the email says it was.
	 * For example, outlook displays a from addresses something like "mailer@wildcat.com (on behalf of sender@foo.com)".
	 * 
	 * @param recipientEmailAddresses  An array of email addresses to send to.
	 * @param ccEmailAddresses An array of email addresses to carbon copy in.
	 * @param senderEmailAddress   Address from which email will be sent
	 *                             (on behalf of rules apply here - see method description above).
	 * @param subject  The subject description shown in an email client.
	 * @param body The email textual content (usually plain text or html).
	 * @param contentType  The mime type of the email body.
	 * @param attachmentFileName
	 */
	public static void writeMail(String[] recipientEmailAddresses,
									String[] ccEmailAddresses,
									String senderEmailAddress,
									String subject,
									String body,
									MimeType contentType, 
									OutputStream out,
									MailAttachment... attachments)
	throws ValidationException {
		MailUtil.writeMail(recipientEmailAddresses,
							ccEmailAddresses,
							senderEmailAddress,
							subject,
							body,
							contentType,
							out,
							attachments);
	}

	/**
	 * 
	 * @param recipientEmailAddresses
	 * @param ccEmailAddresses
	 * @param senderEmailAddress
	 * @param subject
	 * @param body
	 * @param contentType
	 * @param attachmentFileName
	 * @param attachment
	 * @param attachmentType
	 * @throws ValidationException
	 */
	public static void sendMail(String[] recipientEmailAddresses,
									String[] ccEmailAddresses,
									String senderEmailAddress,
									String subject,
									String body,
									MimeType contentType, 
									MailAttachment... attachments)
	throws ValidationException {
		
		MailUtil.sendMail(recipientEmailAddresses,
							ccEmailAddresses,
							senderEmailAddress,
							subject,
							body,
							contentType,
							attachments);
	}

	/**
	 * Run a job once. 
	 * The job disappears from the Scheduler once it is run and a record of the run in placed in admin.Job. 
	 * User must look in admin to see if job was successful.
	 * 
	 * @param job The job to run
	 * @param bean 	The job parameter - can be null.
	 * @param user	The user to run the job as.
	 * 
	 * @throws Exception Anything.
	 */
	public static void runOneShotJob(Job job, Bean parameter, User user)
	throws Exception {
		JobScheduler.runOneShotJob(job, parameter, user);
	}

	/**
	 * Extra parameter gives polling UIs the chance to display the results of the job.
	 * 
	 * @param job The job to run
	 * @param parameter 	The job parameter - can be null.
	 * @param user	The user to run the job as.
	 * @param sleepAtEndInSeconds Set this 5 secs higher than the polling time of the UI
	 * @throws Exception
	 */
	public static void runOneShotJob(Job job, Bean parameter, User user, int sleepAtEndInSeconds)
	throws Exception {
		JobScheduler.runOneShotJob(job, parameter, user, sleepAtEndInSeconds);
	}
	
	/**
	 * 
	 * @param jobSchedule
	 * @param user
	 * @throws Exception
	 */
	public static void scheduleJob(Bean jobSchedule,
									User user)
	throws Exception {
		JobScheduler.scheduleJob(jobSchedule, user);
	}
	
	/**
	 * 
	 * @param jobSchedule
	 * @param customer
	 * @throws Exception
	 */
	public static void unscheduleJob(Bean jobSchedule, Customer customer)
	throws Exception {
		JobScheduler.unscheduleJob(jobSchedule, customer);
	}

	/**
	 * 
	 * @return
	 * @throws Exception
	 */
	public static List<JobDescription> getCustomerRunningJobs()
	throws Exception {
		return JobScheduler.getCustomerRunningJobs();
	}
	
	/**
	 * Tag 1 bean.
	 * 
	 * @param tagId Tag the bean against this tag.
	 * @param bean	The bean to be tagged.
	 * @throws Exception
	 */
	public static void tag(String tagId, Bean bean)
	throws Exception {
		TagUtil.tag(tagId, bean);
	}
	
	/**
	 * Tag 1 bean.
	 * 
	 * @param tagId	Tag the bean against this tag.
	 * @param taggedModuleName	The module name of the bean to be tagged.
	 * @param taggedDocumentName The document name of the bean to be tagged.
	 * @param taggedBizId	The bizId of the bean to be tagged.
	 * @throws Exception
	 */
	public static void tag(String tagId,
							String taggedModuleName,
							String taggedDocumentName,
							String taggedBizId)
	throws Exception {
		TagUtil.tag(tagId, taggedModuleName, taggedDocumentName, taggedBizId);
	}
	
	/**
	 * Remove the bean from this tag.
	 * 
	 * @param tagId	The tag.
	 * @param bean	The bean to untag.
	 * @throws Exception
	 */
	public static void untag(String tagId, Bean bean)
	throws Exception {
		TagUtil.untag(tagId, bean);
	}

	/**
	 * Remove the bean from this tag.
	 * 
	 * @param tagId	The tag.
	 * @param taggedModuleName	The module name of the bean to be untagged.
	 * @param taggedDocumentName The document name of the bean to be untagged.
	 * @param taggedBizId	The bizId of the bean to be untagged.
	 * @throws Exception
	 */
	public static void untag(String tagId,
								String taggedModuleName,
								String taggedDocumentName,
								String taggedBizId)
	throws Exception {
		TagUtil.untag(tagId, taggedModuleName, taggedDocumentName, taggedBizId);
	}

	/**
	 * Create a tag.
	 * 
	 * @param tagName	The name of the tag.
	 * @param visible	Whether the tag should be shown throughout the wildcat UI.
	 * @return	The tagId of the created tag.
	 * @throws Exception
	 */
	public static String createTag(String tagName, boolean visible)
	throws Exception {
		return TagUtil.create(tagName, visible);
	}

	/**
	 * Retrieve the tagId of the named tag.
	 * 
	 * @param tagName  The name of the tag to retrieve.
	 * @return	The corresponding tagId
	 * @throws Exception
	 */
	public static String getTagId(String tagName)
	throws Exception {
		return TagUtil.getTagId(tagName);
	}

	/**
	 * 
	 * @return
	 * @throws Exception
	 */
	public static List<DomainValue> getTags()
	throws Exception {
		return TagUtil.getTags();
	}
	
	/**
	 * Delete a tag.
	 * 
	 * @param tagId	The tagId of the tag to delete.
	 * @throws Exception
	 */
	public static void deleteTag(String tagId)
	throws Exception {
		TagUtil.delete(tagId);
	}

	/**
	 * Tag a bunch of beans.
	 * 
	 * @param tagId	The tag to use.
	 * @param beans	The beans to tag.
	 * @throws Exception
	 */
	public static void tag(String tagId, Iterable<Bean> beans)
	throws Exception {
		TagUtil.tag(tagId, beans);
	}

	/**
	 * Untag (remove) a bunch of beans.
	 * 
	 * @param tagId	The tag to remove from.
	 * @param beans	The beans to untag.
	 * @throws Exception
	 */
	public static void untag(String tagId, Iterable<Bean> beans)
	throws Exception {
		TagUtil.untag(tagId, beans);
	}

	/**
	 * Clear any beans related to the given tag.
	 * 
	 * @param tagId	the given tag
	 * @throws Exception
	 */
	public static void clearTag(String tagId) 
	throws Exception {
		TagUtil.clear(tagId);
	}

	/**
	 * Iterate over the tagged beans.
	 * @param tagId	The tag to iterate.
	 * @return	The beans (a scrolled set).  Each bean is loaded into 1st level cache so beware.
	 * @throws Exception
	 */
	public static Iterable<Bean> iterateTagged(String tagId)
	throws Exception {
		return TagUtil.iterateTagged(tagId);
	}
	
	/**
	 * 
	 * @param user
	 * @param document
	 * @param reportName
	 * @param parameters
	 * @param bean
	 * @param format
	 * @param out
	 * @return
	 * @throws Exception
	 */
	public static JasperPrint runBeanReport(User user,
												Document document,
												String reportName,
												Map<String, Object> parameters,
												Bean bean,
												ReportFormat format,
												OutputStream out)
	throws Exception {
		return ReportUtil.runBeanReport(user, document, reportName, parameters, bean, format, out);
	}

	/**
	 * 
	 * @param user
	 * @param document
	 * @param reportName
	 * @param parameters
	 * @param format
	 * @param out
	 * @return
	 * @throws Exception
	 */
	public static JasperPrint runSQLReport(User user,
											Document document,
											String reportName,
											Map<String, Object> parameters,
											ReportFormat format,
											OutputStream out)
	throws Exception {
		return ReportUtil.runSQLReport(user, document, reportName, parameters, format, out);
	}

	/**
	 * 
	 * @param user
	 * @param document
	 * @param reportName
	 * @param parameters
	 * @param bean
	 * @param format
	 * @param out
	 * @return
	 * @throws Exception
	 */
	public static JasperPrint runReport(User user,
											Document document, 
											String reportName, 
											Map<String, Object> parameters,
											Bean bean,
											ReportFormat format,
											OutputStream out)
	throws Exception {
		return ReportUtil.runReport(user, document, reportName, parameters, bean, format, out);
	}
	
	/**
	 * 
	 * @param jasperPrint
	 * @param format
	 * @param out
	 * @throws JRException
	 */
	public static void runReport(JasperPrint jasperPrint, 
									ReportFormat format,
									OutputStream out)
	throws JRException {
		ReportUtil.runReport(jasperPrint, format, out);
	}
	
	/**
	 * Get a JDBC connection from the wildcat connection pool.
	 * Wildcat uses a container provided JNDI data source for connections.
	 * All servlet and Java EE App stacks can provision this service.
	 * The connection pool used by wildcat is configured in each web app as a context parameter in web.xml.
	 * This method should be used sparingly.
	 * For SQL queries, {@link org.skyve.persistence.Persistence} can be used in conjunction with 
	 * {@link org.skyve.persistence.SQL}.
	 * 
	 * @return a database connection from the container supplied pool.
	 */
	public static Connection getPooledJDBCConnection() throws IllegalStateException {
		Connection result = null;
		try {
			InitialContext ctx = new InitialContext();
			DataSource ds = (DataSource) ctx.lookup(UtilImpl.DATASOURCE);
			result = ds.getConnection();
			result.setAutoCommit(false);
		}
		catch (SQLException e) {
			throw new IllegalStateException("Could not get a database connection", e);
		}
		catch (NamingException e) {
			throw new IllegalStateException("Could not find the JDBC connection pool", e);
		}

		return result;
	}
	
	public static ContentManager newContentManager() {
		return AbstractContentManager.get();
	}
}
