package org.skyve;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;
import javax.websocket.Session;

import org.apache.poi.ss.usermodel.Workbook;
import org.jfree.chart.JFreeChart;
import org.skyve.addin.AddInManager;
import org.skyve.addin.DefaultAddInManager;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.bizport.POISheet;
import org.skyve.impl.bizport.POIWorkbook;
import org.skyve.impl.bizport.StandardGenerator;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.dataaccess.sql.SQLDataAccessImpl;
import org.skyve.impl.generate.charts.JFreeChartGenerator;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.security.SkyveLegacyPasswordEncoder;
import org.skyve.impl.util.MailUtil;
import org.skyve.impl.util.ReportParameters;
import org.skyve.impl.util.ReportUtil;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.TagUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.JobDescription;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.report.ReportFormat;
import org.skyve.util.JSON;
import org.skyve.util.Mail;
import org.skyve.util.MailAttachment;
import org.skyve.util.PushMessage;
import org.skyve.util.Util;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.DelegatingPasswordEncoder;
import org.springframework.security.crypto.password.Pbkdf2PasswordEncoder;
import org.springframework.security.crypto.scrypt.SCryptPasswordEncoder;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperPrint;

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
	 * @param mail	The email to write.
	 * @param out	The stream to write to.
	 */
	public static void writeMail(Mail mail, OutputStream out) {
		MailUtil.writeMail(mail, out);
	}

	/**
	 * Send an email.
	 * 
	 * @param mail	The email to send.
	 */
	public static void sendMail(Mail mail) {
		MailUtil.sendMail(mail);
	}

	/**
	 * Push a message to connected client user interfaces.
	 */
	public static void push(PushMessage message) {
		// Note Sessions are thread-safe
		Set<String> userIds = message.getUserIds();
		boolean broadcast = userIds.isEmpty();
		for (Session session : PushMessage.SESSIONS) {
			if (session.isOpen()) {
				if (broadcast) {
					session.getAsyncRemote().sendText(JSON.marshall(message.getItems()));
				}
				else {
					Object userId = session.getUserProperties().get("user");
					if ((userId == null) || userIds.contains(userId)) {
						session.getAsyncRemote().sendText(JSON.marshall(message.getItems()));
					}
				}
			}
		}
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
	 * Generate an image of a chart.
	 * @param data
	 * @param pixelWidth
	 * @param pixelHeight
	 * @return
	 */
	public static BufferedImage chartImage(ChartType type, ChartData data, int pixelWidth, int pixelHeight) {
		return new JFreeChartGenerator(data, pixelWidth, pixelHeight).image(type);
	}
	
	/**
	 * Generate a chart.
	 * @param data
	 * @param pixelWidth
	 * @param pixelHeight
	 * @return
	 */
	public static JFreeChart chart(ChartType type, ChartData data, int pixelWidth, int pixelHeight) {
		return new JFreeChartGenerator(data, pixelWidth, pixelHeight).chart(type);
	}
	
	/**
	 * Create a new chart generator.
	 * @param data
	 * @param pixelWidth
	 * @param pixelHeight
	 * @return
	 */
	public static JFreeChartGenerator newChartGenerator(ChartData data, int pixelWidth, int pixelHeight) {
		return new JFreeChartGenerator(data, pixelWidth, pixelHeight);
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
	 * @param user
	 * @param reportParameters
	 * @param format
	 * @param out
	 * @return
	 * @throws Exception
	 */
	public static List<JasperPrint> runReport(User user, List<ReportParameters> reportParameters, ReportFormat format, OutputStream out) throws Exception {
		return ReportUtil.runReport(user, reportParameters, format, out);
	}

	/**
	 *
	 * @param jasperPrint
	 * @param format
	 * @param out
	 * @throws JRException
	 */
	public static void runReport(JasperPrint jasperPrint, ReportFormat format, OutputStream out) throws Exception {
		ReportUtil.runReport(jasperPrint, format, out);
	}

	/**
	 *
	 * @param jasperPrintList
	 * @param format
	 * @param out
	 * @throws JRException
	 */
	public static void runReport(List<JasperPrint> jasperPrintList, ReportFormat format, OutputStream out) throws Exception {
		ReportUtil.runReport(jasperPrintList, format, out);
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
	@SuppressWarnings("resource")
	public static ContentManager newContentManager() {
		ContentManager result = DefaultAddInManager.get().getExtension(ContentManager.class);
		if (result == null) {
			if (AbstractContentManager.IMPLEMENTATION_CLASS == null) {
				throw new DomainException("No content manager addin detected and \"factories.contentManagerClass\" is not defined in the Skyve configuration");
			}
			result = AbstractContentManager.get();
		}
		return result;
	}

	public static AddInManager getAddInManager() {
		return DefaultAddInManager.get();
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
	public static MailAttachment getMailAttachmentFromReport(String reportModuleName,
																String reportDocumentName,
																String reportName,
																Map<String, Object> parameters)
	throws Exception {
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

	/**
	 * Returns a mail attachment from a Jasper report as a PDF
	 *
	 * @param reportParameters
	 */
	public static MailAttachment getMailAttachmentFromReport(List<ReportParameters> reportParameters) throws Exception {
		if (reportParameters.isEmpty()) {
			throw new IllegalArgumentException("There must be at least 1 report to generate.");
		}

		MailAttachment result = new MailAttachment();

		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();

		ByteArrayOutputStream out = new ByteArrayOutputStream();

		EXT.runReport(user, reportParameters, ReportFormat.pdf, out);
		byte[] reportBytes = out.toByteArray();

		result.setAttachmentFileName(reportParameters.get(0).getReportName());
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
	
	/**
	 * Provide a hash of a clear text password.
	 * @param clearText
	 * @return	The encoded password.
	 */
	public static String hashPassword(String clearText) {
		String result = null;

		String passwordHashingAlgorithm = Util.getPasswordHashingAlgorithm();
		// Legacy hashing with no SALT
		if ("MD5".equals(passwordHashingAlgorithm) || "SHA1".equals(passwordHashingAlgorithm)) {
			result = SkyveLegacyPasswordEncoder.encode(clearText, passwordHashingAlgorithm);
		}
		else if ("bcrypt".equals(passwordHashingAlgorithm)) {
			result = "{bcrypt}" + new BCryptPasswordEncoder().encode(clearText);
		}
		else if ("pbkdf2".equals(passwordHashingAlgorithm)) {
			result = "{pbkdf2}" + new Pbkdf2PasswordEncoder().encode(clearText);
		}
		else if ("scrypt".equals(passwordHashingAlgorithm)) {
			result = "{scrypt}" + new SCryptPasswordEncoder().encode(clearText);
		}
		else {
			throw new DomainException(passwordHashingAlgorithm + " not supported");
		}
		
		return result;
	}

	/**
	 * Check a hash against a clear text password.
	 * @param	clearText
	 * @param	The encoded password.
	 * @return	true if it matches, or false if it doesn't
	 */
	public static boolean checkPassword(String clearText, String hashedPassword) {
		DelegatingPasswordEncoder dpe = (DelegatingPasswordEncoder) PasswordEncoderFactories.createDelegatingPasswordEncoder();
		dpe.setDefaultPasswordEncoderForMatches(new SkyveLegacyPasswordEncoder());
		return dpe.matches(clearText, hashedPassword);
	}
	
	/**
	 * Inject a bootstrap user according to the settings in the .json Bootstrap stanza
	 * 
	 * @param p
	 * @throws Exception
	 */
	public static void bootstrap(Persistence p) throws Exception {
		User u = p.getUser();
		Customer c = u.getCustomer();
		Module adminMod = c.getModule(SQLMetaDataUtil.ADMIN_MODULE_NAME);
		Document contactDoc = adminMod.getDocument(c, SQLMetaDataUtil.CONTACT_DOCUMENT_NAME);
		Document userDoc = adminMod.getDocument(c, SQLMetaDataUtil.USER_DOCUMENT_NAME);
		Document userRoleDoc = adminMod.getDocument(c, SQLMetaDataUtil.USER_ROLE_DOCUMENT_NAME);
		DocumentQuery q = p.newDocumentQuery(userDoc);
		q.getFilter().addEquals(SQLMetaDataUtil.USER_NAME_PROPERTY_NAME, UtilImpl.BOOTSTRAP_USER);
		PersistentBean user = q.beanResult();
		if (user == null) {
			UtilImpl.LOGGER.info(String.format("CREATING BOOTSTRAP USER %s/%s (%s)",
												UtilImpl.BOOTSTRAP_CUSTOMER,
												UtilImpl.BOOTSTRAP_USER,
												UtilImpl.BOOTSTRAP_EMAIL));

			// Create user
			user = userDoc.newInstance(u);
			u.setId(user.getBizId());
			user.setBizUserId(u.getId());
			BindUtil.set(user, SQLMetaDataUtil.USER_NAME_PROPERTY_NAME, UtilImpl.BOOTSTRAP_USER);
			BindUtil.set(user, SQLMetaDataUtil.PASSWORD_PROPERTY_NAME, u.getPasswordHash());
			BindUtil.set(user, SQLMetaDataUtil.PASSWORD_LAST_CHANGED_PROPERTY_NAME, new DateTime());

			// Create contact
			Bean contact = contactDoc.newInstance(u);
			BindUtil.set(contact, SQLMetaDataUtil.NAME_PROPERTY_NAME, UtilImpl.BOOTSTRAP_USER);
			BindUtil.convertAndSet(contact, SQLMetaDataUtil.CONTACT_TYPE_PROPERTY_NAME, "Person");
			BindUtil.set(contact, SQLMetaDataUtil.EMAIL1_PROPERTY_NAME, UtilImpl.BOOTSTRAP_EMAIL);

			BindUtil.set(user, SQLMetaDataUtil.CONTACT_PROPERTY_NAME, contact);

			// Add roles
			@SuppressWarnings("unchecked")
			List<Bean> roles = (List<Bean>) BindUtil.get(user, SQLMetaDataUtil.ROLES_PROPERTY_NAME);
			for (Module m : c.getModules()) {
				String moduleName = m.getName();
				for (Role r : m.getRoles()) {
					Bean role = userRoleDoc.newInstance(u);
					BindUtil.set(role, ChildBean.PARENT_NAME, user);
					BindUtil.set(role, SQLMetaDataUtil.ROLE_NAME_PROPERTY_NAME, String.format("%s.%s", moduleName, r.getName()));
					roles.add(role);
				}
			}

			// Save the bootstrap user
			p.save(user);
		}
		else {
			UtilImpl.LOGGER.info(String.format("BOOTSTRAP USER %s/%s ALREADY EXISTS",
												UtilImpl.BOOTSTRAP_CUSTOMER,
												UtilImpl.BOOTSTRAP_USER));
		}
	}	
}
