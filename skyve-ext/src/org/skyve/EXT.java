package org.skyve;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperPrint;

import org.apache.poi.ss.usermodel.Workbook;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.bizport.POISheet;
import org.skyve.impl.bizport.POIWorkbook;
import org.skyve.impl.bizport.StandardGenerator;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.dataaccess.sql.SQLDataAccessImpl;
import org.skyve.impl.util.MailUtil;
import org.skyve.impl.util.ReportUtil;
import org.skyve.impl.util.TagUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.JobDescription;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.Persistence;
import org.skyve.report.ReportFormat;
import org.skyve.util.MailAttachment;

/**
 * The central factory for creating all objects required in skyve ext.
 * See {@link org.skyve.CORE} for creating objects implemented in the skyve core API.
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
	 * @param customer
	 *            The current customer (usually for the logged in user).
	 * @param workbook
	 *            The POI workbook representing an existing excel file.
	 * @param e
	 *            This exception is will contain any errors encountered during
	 *            population of the bizport workbook data structure. This method
	 *            will automatically throw <code>e</code> if any errors are
	 *            added to it.
	 */
	public static BizPortWorkbook newBizPortWorkbook(Customer customer, Workbook workbook, UploadException e) {
		return new POIWorkbook(customer, workbook, e);
	}

	/**
	 * BizPort workbook sheet factory method for creating a new sheet. Note that
	 * the sheet is not added to any workbook. See
	 * {@link org.skyve.bizport.BizPortWorkbook#addSheet} for this.
	 * 
	 * @param title
	 *            The title of the sheet, used to name the excel sheet tab.
	 */
	public static BizPortSheet newBizPortSheet(String title) {
		return new POISheet(title);
	}

	/**
	 * Creates a new standard generator based on a driving document.
	 * 
	 * @param customer
	 *            The relevant customer for the generation.
	 * @param document
	 *            The driving document for the generation. The top-level
	 *            document which the bizport excel document will model.
	 * @param exclusions
	 *            Bindings (relative to driving document) to exclude from the
	 *            generation. Each binding will stop the recursive generation
	 *            processing when it is satisfied.
	 */
	public static StandardGenerator newBizPortStandardGenerator(Customer customer, Document document, String... exclusions) {
		return new StandardGenerator(customer, document, exclusions);
	}

	/**
	 * Write an email to the output stream <code>out</code> in MIME RFC 822
	 * format with multiple attachments.
	 * 
	 * Outlook can load this format. Note - "on behalf of"... If the sender
	 * email address differs from the skyve configured email credentials then,
	 * depending on the email client receiving the email at the destination, the
	 * email sending address may display with an indication that it wasn't
	 * actually sent from the email account the email says it was. For example,
	 * outlook displays a from addresses something like
	 * "mailer@skyve.com (on behalf of sender@foo.com)".
	 * 
	 * @param recipientEmailAddresses
	 *            An array of email addresses to send to.
	 * @param ccEmailAddresses
	 *            An array of email addresses to carbon copy in.
	 * @param senderEmailAddress
	 *            Address from which email will be sent (on behalf of rules
	 *            apply here - see method description above).
	 * @param subject
	 *            The subject description shown in an email client.
	 * @param body
	 *            The email textual content (usually plain text or html).
	 * @param contentType
	 *            The mime type of the email body.
	 * @param attachmentFileName
	 */
	public static void writeMail(String[] recipientEmailAddresses, String[] ccEmailAddresses, String[] bccEmailAddresses, String senderEmailAddress, String subject, String body, MimeType contentType, OutputStream out, MailAttachment... attachments) {
		MailUtil.writeMail(recipientEmailAddresses, ccEmailAddresses, bccEmailAddresses, senderEmailAddress, subject, body, contentType, out, attachments);
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
	 */
	public static void sendMail(String[] recipientEmailAddresses, String[] ccEmailAddresses, String[] bccEmailAddresses, String senderEmailAddress, String subject, String body, MimeType contentType, MailAttachment... attachments) {
		MailUtil.sendMail(recipientEmailAddresses, ccEmailAddresses, bccEmailAddresses, senderEmailAddress, subject, body, contentType, attachments);
	}

	/**
	 * Run a job once. The job disappears from the Scheduler once it is run and
	 * a record of the run in placed in admin.Job. User must look in admin to
	 * see if job was successful.
	 * 
	 * @param job
	 *            The job to run
	 * @param bean
	 *            The job parameter - can be null.
	 * @param user
	 *            The user to run the job as.
	 * 
	 * @throws Exception
	 *             Anything.
	 */
	public static void runOneShotJob(JobMetaData job, Bean parameter, User user) throws Exception {
		JobScheduler.runOneShotJob(job, parameter, user);
	}

	/**
	 * Extra parameter gives polling UIs the chance to display the results of
	 * the job.
	 * 
	 * @param job
	 *            The job to run
	 * @param parameter
	 *            The job parameter - can be null.
	 * @param user
	 *            The user to run the job as.
	 * @param sleepAtEndInSeconds
	 *            Set this 5 secs higher than the polling time of the UI
	 * @throws Exception
	 */
	public static void runOneShotJob(JobMetaData job, Bean parameter, User user, int sleepAtEndInSeconds) throws Exception {
		JobScheduler.runOneShotJob(job, parameter, user, sleepAtEndInSeconds);
	}

	/**
	 * Run a job once at a certain date and time. 
	 * The job disappears from the Scheduler once it is run and a record of the run in placed in admin.Job. 
	 * User must look in admin to see if job was successful.
	 * 
	 * @param job The job to run
	 * @param parameter 	The job parameter - can be null.
	 * @param user	The user to run the job as.
	 * @param when	The date/time to run the job at.
	 * 
	 * @throws Exception Anything.
	 */
	public static void scheduleOneShotJob(JobMetaData job, Bean parameter, User user, Date when)
	throws Exception {
		JobScheduler.scheduleOneShotJob(job, parameter, user, when);
	}

	/**
	 * 
	 * @return
	 * @throws Exception
	 */
	public static List<JobDescription> getCustomerRunningJobs() throws Exception {
		return JobScheduler.getCustomerRunningJobs();
	}

	/**
	 * Tag 1 bean.
	 * 
	 * @param tagId
	 *            Tag the bean against this tag.
	 * @param bean
	 *            The bean to be tagged.
	 * @throws Exception
	 */
	public static void tag(String tagId, Bean bean) throws Exception {
		TagUtil.tag(tagId, bean);
	}

	/**
	 * Tag 1 bean.
	 * 
	 * @param tagId
	 *            Tag the bean against this tag.
	 * @param taggedModuleName
	 *            The module name of the bean to be tagged.
	 * @param taggedDocumentName
	 *            The document name of the bean to be tagged.
	 * @param taggedBizId
	 *            The bizId of the bean to be tagged.
	 * @throws Exception
	 */
	public static void tag(String tagId, String taggedModuleName, String taggedDocumentName, String taggedBizId) throws Exception {
		TagUtil.tag(tagId, taggedModuleName, taggedDocumentName, taggedBizId);
	}

	/**
	 * Remove the bean from this tag.
	 * 
	 * @param tagId
	 *            The tag.
	 * @param bean
	 *            The bean to untag.
	 * @throws Exception
	 */
	public static void untag(String tagId, Bean bean) throws Exception {
		TagUtil.untag(tagId, bean);
	}

	/**
	 * Remove the bean from this tag.
	 * 
	 * @param tagId
	 *            The tag.
	 * @param taggedModuleName
	 *            The module name of the bean to be untagged.
	 * @param taggedDocumentName
	 *            The document name of the bean to be untagged.
	 * @param taggedBizId
	 *            The bizId of the bean to be untagged.
	 * @throws Exception
	 */
	public static void untag(String tagId, String taggedModuleName, String taggedDocumentName, String taggedBizId) throws Exception {
		TagUtil.untag(tagId, taggedModuleName, taggedDocumentName, taggedBizId);
	}

	/**
	 * Create a tag.
	 * 
	 * @param tagName
	 *            The name of the tag.
	 * @param visible
	 *            Whether the tag should be shown throughout the skyve UI.
	 * @return The tagId of the created tag.
	 * @throws Exception
	 */
	public static String createTag(String tagName, boolean visible) throws Exception {
		return TagUtil.create(tagName, visible);
	}

	/**
	 * Retrieve the tagId of the named tag.
	 * 
	 * @param tagName
	 *            The name of the tag to retrieve.
	 * @return The corresponding tagId
	 * @throws Exception
	 */
	public static String getTagId(String tagName) throws Exception {
		return TagUtil.getTagId(tagName);
	}

	/**
	 * 
	 * @return
	 * @throws Exception
	 */
	public static List<DomainValue> getTags() throws Exception {
		return TagUtil.getTags();
	}

	/**
	 * Delete a tag.
	 * 
	 * @param tagId
	 *            The tagId of the tag to delete.
	 * @throws Exception
	 */
	public static void deleteTag(String tagId) throws Exception {
		TagUtil.delete(tagId);
	}

	/**
	 * Tag a bunch of beans.
	 * 
	 * @param tagId
	 *            The tag to use.
	 * @param beans
	 *            The beans to tag.
	 * @throws Exception
	 */
	public static void tag(String tagId, Iterable<Bean> beans) throws Exception {
		TagUtil.tag(tagId, beans);
	}

	/**
	 * Untag (remove) a bunch of beans.
	 * 
	 * @param tagId
	 *            The tag to remove from.
	 * @param beans
	 *            The beans to untag.
	 * @throws Exception
	 */
	public static void untag(String tagId, Iterable<Bean> beans) throws Exception {
		TagUtil.untag(tagId, beans);
	}

	/**
	 * Clear any beans related to the given tag.
	 * 
	 * @param tagId
	 *            the given tag
	 * @throws Exception
	 */
	public static void clearTag(String tagId) throws Exception {
		TagUtil.clear(tagId);
	}

	/**
	 * Iterate over the tagged beans.
	 * 
	 * @param tagId
	 *            The tag to iterate.
	 * @return The beans (a scrolled set). Each bean is loaded into 1st level
	 *         cache so beware.
	 * @throws Exception
	 */
	public static Iterable<Bean> iterateTagged(String tagId) throws Exception {
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
	public static JasperPrint runBeanReport(User user, Document document, String reportName, Map<String, Object> parameters, Bean bean, ReportFormat format, OutputStream out) throws Exception {
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
	public static JasperPrint runSQLReport(User user, Document document, String reportName, Map<String, Object> parameters, ReportFormat format, OutputStream out) throws Exception {
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
	public static JasperPrint runReport(User user, Document document, String reportName, Map<String, Object> parameters, Bean bean, ReportFormat format, OutputStream out) throws Exception {
		return ReportUtil.runReport(user, document, reportName, parameters, bean, format, out);
	}

	/**
	 * 
	 * @param jasperPrint
	 * @param format
	 * @param out
	 * @throws JRException
	 */
	public static void runReport(JasperPrint jasperPrint, ReportFormat format, OutputStream out) throws JRException {
		ReportUtil.runReport(jasperPrint, format, out);
	}

	/**
	 * Get a JDBC connection from the skyve data store definition. 
	 * Skyve uses a container provided JNDI data source or driver/url/user/pass combinationfor connections. 
	 * All servlet and Java EE App stacks can provision this service. 
	 * The connection pool used by skyve is configured in each web app in the json config.
	 * This method should be used sparingly. For SQL queries,
	 * {@link org.skyve.persistence.Persistence} can be used in conjunction with
	 * {@link org.skyve.persistence.SQL}.
	 * 
	 * @return a database connection from the container supplied pool.
	 */
	@SuppressWarnings("resource")
	public static Connection getDataStoreConnection(DataStore dataStore) throws IllegalStateException {
		Connection result = null;
		try {
			String jndiDataSourceName = dataStore.getJndiDataSourceName();
			if (jndiDataSourceName == null) {
				Class.forName(dataStore.getJdbcDriverClassName());
				Properties connectionProps = new Properties();
				String value = dataStore.getUserName();
				if (value != null) {
					connectionProps.put("user", value);
				}
				value = dataStore.getPassword();
				if (value != null) {
					connectionProps.put("password", value);
				}
				result = DriverManager.getConnection(dataStore.getJdbcUrl(), connectionProps);
			}
			else {
				InitialContext ctx = new InitialContext();
				DataSource ds = (DataSource) ctx.lookup(jndiDataSourceName);
				result = ds.getConnection();
			}

			if (result != null) {
				result.setAutoCommit(false);
			}
		}
		catch (SQLException e) {
			throw new IllegalStateException("Could not get a database connection", e);
		}
		catch (NamingException e) {
			throw new IllegalStateException("Could not find the JDBC connection pool", e);
		} 
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException("Could not instantiate the JDBC driver", e);
		}

		return result;
	}
	
	// Not a CDI provider as it is auto-closeable 
	public static Connection getDataStoreConnection() {
		return getDataStoreConnection(UtilImpl.DATA_STORE);
	}

	// Not a CDI provider as it is auto-closeable 
	public static ContentManager newContentManager() {
		return AbstractContentManager.get();
	}

	/**
	 * Returns a mail attachment from bean content attribute
	 * 
	 * @param beanContent
	 * @param attachmentName
	 * @throws Exception
	 */
	public static MailAttachment getMailAttachmentFromContent(String beanContent, String attachmentName) throws Exception {
		MailAttachment result = new MailAttachment();
		try (ContentManager cm = EXT.newContentManager()) {
			if (beanContent != null) {
				AttachmentContent content = cm.get(beanContent);
				if (content == null) {
					throw new DomainException("The content for the attachment can't be retrieved - re-attach the content and try again.");
				}
				byte[] fileBytes = content.getContentBytes();

				result.setAttachmentFileName(attachmentName);
				result.setAttachment(fileBytes);
				result.setAttachmentMimeType(content.getMimeType());
			}
		}

		return result;
	}

	/**
	 * Returns a mail attachment from a Jasper report as a PDF
	 * 
	 * @param reportModuleName
	 * @param reportDocumentName
	 * @param reportName
	 * @param parameters
	 */
	public static MailAttachment getMailAttachmentFromReport(String reportModuleName, String reportDocumentName, String reportName, Map<String, Object> parameters) throws Exception {
		MailAttachment result = new MailAttachment();

		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Document document = customer.getModule(reportModuleName).getDocument(customer, reportDocumentName);

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		EXT.runSQLReport(user, document, reportName, parameters, ReportFormat.pdf, out);
		byte[] reportBytes = out.toByteArray();

		result.setAttachmentFileName(reportName);
		result.setAttachment(reportBytes);
		result.setAttachmentMimeType(MimeType.pdf);

		return result;
	}
	
	// NB Not a CDI provider as it is auto-closeable 
	public static SQLDataAccess newSQLDataAccess() {
		return new SQLDataAccessImpl(UtilImpl.DATA_STORE);
	}
	
	public static SQLDataAccess newSQLDataAccess(DataStore dataStore) {
		return new SQLDataAccessImpl(dataStore);
	}
}
