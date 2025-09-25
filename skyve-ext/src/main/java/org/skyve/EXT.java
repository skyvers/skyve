package org.skyve;

import java.awt.image.BufferedImage;
import java.io.InputStream;
import java.io.OutputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.jfree.chart.JFreeChart;
import org.skyve.addin.AddInManager;
import org.skyve.bizport.BizPortSheet;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.cache.Caching;
import org.skyve.content.ContentManager;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.AccessException;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.addin.PF4JAddInManager;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.bizport.POISheet;
import org.skyve.impl.bizport.POIWorkbook;
import org.skyve.impl.bizport.StandardGenerator;
import org.skyve.impl.bizport.StandardLoader;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.dataaccess.sql.SQLDataAccessImpl;
import org.skyve.impl.generate.charts.JFreeChartGenerator;
import org.skyve.impl.geoip.GeoIPServiceStaticSingleton;
import org.skyve.impl.job.JobSchedulerStaticSingleton;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.report.DefaultReporting;
import org.skyve.impl.sms.SMSServiceStaticSingleton;
import org.skyve.impl.tag.DefaultTagManager;
import org.skyve.impl.util.MailUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.HttpServletRequestResponse;
import org.skyve.impl.web.WebContainer;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.RDBMSDynamicPersistenceListModel;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.report.Reporting;
import org.skyve.tag.TagManager;
import org.skyve.util.GeoIPService;
import org.skyve.util.Mail;
import org.skyve.util.PushMessage;
import org.skyve.util.PushMessage.PushMessageReceiver;
import org.skyve.util.SMSService;
import org.skyve.util.SecurityUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.crypto.password.PasswordEncoder;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * The central factory for creating all objects required in skyve ext.
 * See {@link org.skyve.CORE} for creating objects implemented in the skyve core API.
 */
public class EXT {

    private static final Logger LOGGER = LoggerFactory.getLogger(EXT.class);

	/**
	 * Disallow instantiation
	 */
	private EXT() {
		// no-op
	}

	/**
	 * For Scheduling Jobs, Background Tasks and Reports.
	 * @return A JobScheduler
	 */
	public static @Nonnull JobScheduler getJobScheduler() {
		return JobSchedulerStaticSingleton.get();
	}
	
	/**
	 * For tag operations.
	 * @return A tag manager
	 */
	public static @Nonnull TagManager getTagManager() {
		return DefaultTagManager.get();
	}
	
	/**
	 * Get a reporting service.
	 * @return	A reporting service.
	 */
	public static @Nonnull Reporting getReporting() {
		return DefaultReporting.get();
	}
	
	/**
	 * Get a cache manager
	 * @ return A cache manager
	 */
	public static @Nonnull Caching getCaching() {
		return DefaultCaching.get();
	}
	
	/**
	 * Get an add-in manager
	 * @ return An add-in manager
	 */
	public static @Nonnull AddInManager getAddInManager() {
		return PF4JAddInManager.get();
	}

	/**
	 * Get a geo-ip service
	 * @ return A geo-ip service
	 */
	public static @Nonnull GeoIPService getGeoIPService() {
		return GeoIPServiceStaticSingleton.get();
	}

	/**
	 * Get an SMS (text message) service
	 * @return An SMS service
	 */
	public static @Nonnull SMSService getSMSService() {
		return SMSServiceStaticSingleton.get();
	}

	/**
	 * New file factory method for creating a BizPort excel workbook.
	 * 
	 * @return The new workbook.
	 */
	public static @Nonnull BizPortWorkbook newBizPortWorkbook(boolean ooxmlFormat) {
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
	public static @Nonnull BizPortWorkbook newBizPortWorkbook(Customer customer, Workbook workbook, UploadException e) {
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
	public static @Nonnull BizPortSheet newBizPortSheet(String title) {
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
	public static @Nonnull StandardGenerator newBizPortStandardGenerator(@Nonnull Customer customer, @Nonnull Document document, @Nonnull String... exclusions) {
		return new StandardGenerator(customer, document, exclusions);
	}
	
	/**
	 * Create a new standard loader based on the wookrbook provided.
	 * 
	 * @param workbook	The workbook to load.
	 * @param problems	A fresh UploadException that can be thrown to provide messages.
	 * @return	The StandardLoader.
	 */
	public static @Nonnull StandardLoader newBizPortStandardLoader(@Nonnull BizPortWorkbook workbook, @Nonnull UploadException problems) {
		return new StandardLoader(workbook, problems);
	}
	
	/**
	 * Export all persisted instances of the given document from the data store.
	 * 
	 * @param document	The document to export.
	 * @return	A workbook.
	 */
	public static @Nonnull BizPortWorkbook standardBizExport(@Nonnull final Document document) {
		BizPortWorkbook result = EXT.newBizPortWorkbook(true);
		Customer c = CORE.getCustomer();
		StandardGenerator jenny = EXT.newBizPortStandardGenerator(c, document);
		jenny.generateStructure(result);
		result.materialise();
		DocumentQuery query = CORE.getPersistence().newDocumentQuery(document);
		try (AutoClosingIterable<Bean> i = query.beanIterable()) {
			jenny.generateData(result, i);
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}

		return result;
	}

	/**
	 * Convenience constructor for {@link #standardBizExport(Document)}.
	 */
	public static @Nonnull BizPortWorkbook standardBizExport(@Nonnull final String moduleName,
																@Nonnull final String documentName) {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(moduleName);
		Document d = m.getDocument(c, documentName);
		return standardBizExport(d);
	}

	/**
	 * Export the bean given to a woorkbook.
	 * Just the one bean and its object graph are exported.
	 * Hierarchical beans are exported with their children.
	 * 
	 * @param bean	The bean to export.
	 * @return	The workbook.
	 */
	public static @Nonnull BizPortWorkbook standardBizExport(@Nonnull final Bean bean) {
		BizPortWorkbook result = EXT.newBizPortWorkbook(true);
		Customer c = CORE.getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		StandardGenerator jenny = EXT.newBizPortStandardGenerator(c, d);
		jenny.generateStructure(result);
		result.materialise();
		jenny.generateData(result, bean);

		return result;
	}

	/**
	 * Import a workbook from an InputStream.
	 * 
	 * @param is	The input stream of the workbook.
	 * @param problems	A fresh UploadException that can be thrown to provide messages.
	 * @throws Exception	When something unforseen goes wrong
	 */
	public static void standardBizImport(@Nonnull InputStream is, @Nonnull UploadException problems) throws Exception {
		final Customer c = CORE.getCustomer();
		try (Workbook wb = WorkbookFactory.create(is)) {
			BizPortWorkbook workbook = EXT.newBizPortWorkbook(c, wb, problems);
			standardBizImport(workbook, problems);
		}
	}

	/**
	 * Import a workbook.
	 * 
	 * @param workbook	The workbook to import.
	 * @param problems	A fresh UploadException that can be thrown to provide messages.
	 * @throws Exception	When something unforseen goes wrong
	 */
	public static void standardBizImport(@Nonnull BizPortWorkbook workbook, @Nonnull UploadException problems) throws Exception {
		final Persistence p = CORE.getPersistence();
		final Customer c = p.getUser().getCustomer();

		StandardLoader loader = EXT.newBizPortStandardLoader(workbook, problems);
		List<Bean> beans = loader.populate(p);

		for (String key : loader.getBeanKeys()) {
			Bean bean = loader.getBean(key);
			if (bean == null) {
				loader.addError(c, bean, new IllegalStateException("Bean with key " + key + " not found"));
			}
			else {
				Module module = c.getModule(bean.getBizModule());
				Document document = module.getDocument(c, bean.getBizDocument());
	
				try {
					p.preMerge(document, (PersistentBean) bean);
				}
				catch (DomainException e) {
					loader.addError(c, bean, e);
				}
			}
		}

		// throw if we have errors found
		if (problems.hasErrors()) {
			throw problems;
		}

		// do the insert as 1 operation, bugging out and adding 1 error only if there is a problem
		PersistentBean persistentBean = null;
		try {
			for (Bean bean : beans) {
				persistentBean = (PersistentBean) bean;
				persistentBean = p.save(persistentBean);
			}
		}
		catch (DomainException e) {
			if (persistentBean != null) {
				loader.addError(c, persistentBean, e);
			}
			throw problems;
		}
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
	public static void writeMail(@Nonnull Mail mail, @Nonnull OutputStream out) {
		MailUtil.writeMail(mail, out);
	}

	/**
	 * Send an email.
	 * 
	 * @param mail	The email to send.
	 */
	public static void sendMail(@Nonnull Mail mail) {
		MailUtil.sendMail(mail);
	}

	/**
	 * Push a message to connected client user interfaces.
	 */
	public static void push(@Nonnull PushMessage message) {

		LOGGER.debug("Pushing message: {}", message);

		Set<String> userIds = message.getUserIds();
		boolean broadcast = userIds.isEmpty();

		for (PushMessageReceiver msgReceiver : PushMessage.RECEIVERS) {

			if (broadcast) {
				msgReceiver.sendMessage(message);
			} else {

				String userId = msgReceiver.forUserId();
				if (userIds.contains(userId)) {
					msgReceiver.sendMessage(message);
				}
			}
		}
	}

	/**
	 * Generate an image of a chart.
	 * 
	 * @param type	The chart type
	 * @param data	The chart data
	 * @param pixelWidth	Required width of the image
	 * @param pixelHeight	Required height of the image
	 * @return	A buffered image of the chart.
	 */
	public static @Nonnull BufferedImage chartImage(@Nonnull ChartType type,
														@Nonnull ChartData data,
														int pixelWidth,
														int pixelHeight) {
		return new JFreeChartGenerator(data, pixelWidth, pixelHeight).image(type);
	}
	
	/**
	 * Generate a chart.
	 * 
	 * @param type	The chart type
	 * @param data	The chart data
	 * @param pixelWidth	Required width of the image
	 * @param pixelHeight	Required height of the image
	 * @return	A chart.
	 */
	public static @Nonnull JFreeChart chart(@Nonnull ChartType type,
												@Nonnull ChartData data,
												int pixelWidth,
												int pixelHeight) {
		return new JFreeChartGenerator(data, pixelWidth, pixelHeight).chart(type);
	}
	
	/**
	 * Create a new chart generator.
	 * 
	 * @param data	The chart data
	 * @param pixelWidth	Required width of the image
	 * @param pixelHeight	Required height of the image
	 * @return A chart generator.
	 */
	public static @Nonnull JFreeChartGenerator newChartGenerator(@Nonnull ChartData data, int pixelWidth, int pixelHeight) {
		return new JFreeChartGenerator(data, pixelWidth, pixelHeight);
	}
	
	/**
	 * Get a JDBC connection from the skyve data store definition. 
	 * Skyve uses a container provided JNDI data source or driver/url/user/pass combination for connections. 
	 * All servlet and Java EE App stacks can provision this service. 
	 * The connection pool used by skyve is configured in each web app in the json config.
	 * This method should be used sparingly. For SQL queries,
	 * {@link org.skyve.persistence.Persistence} can be used in conjunction with
	 * {@link org.skyve.persistence.SQL}.
	 * 
	 * @return a database connection from the container supplied pool.
	 */
	public static @Nonnull Connection getDataStoreConnection(@Nonnull DataStore dataStore) {
		Connection result = null;
		try {
			String jndiDataSourceName = dataStore.getJndiDataSourceName();
			if (jndiDataSourceName == null) {
				// Required for any JDBC drivers < JDBC 4.0.
				// It required class loading AND initialisation, not just loading - ie Thread.currentThread().getContextClassLoader().loadClass(...)
				Class.forName(dataStore.getJdbcDriverClassName(), true, Thread.currentThread().getContextClassLoader());
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

			result.setAutoCommit(false);
		}
		catch (SQLException e) {
			throw new DomainException("Could not get a database connection", e);
		}
		catch (NamingException e) {
			throw new DomainException("Could not find the JDBC connection pool", e);
		} 
		catch (ReflectiveOperationException e) {
			throw new DomainException("Could not instantiate the JDBC driver", e);
		}

		return result;
	}
	
	/**
	 * Get a connection to the default data store.
	 * Note that this is not a CDI provider as it is auto-closeable.
	 * 
	 * @return A connection.
	 */
	public static @Nonnull Connection getDataStoreConnection() {
		return getDataStoreConnection(UtilImpl.DATA_STORE);
	}

	/**
	 * Get a new content manager instance for the content implementation.
	 * Note that this is not a CDI provider as it is auto-closeable.
	 * 
	 * @return A content manger.
	 * @throws DomainException	If a content manager cannot be created.
	 */
	@SuppressWarnings("resource")
	public static @Nonnull ContentManager newContentManager()
	throws DomainException {
		final ContentManager result = (AbstractContentManager.IMPLEMENTATION_CLASS == null) ?
										PF4JAddInManager.get().getExtension(ContentManager.class) :
										AbstractContentManager.get();
		if (result == null) {
			throw new DomainException("No content manager addin detected and \"factories.contentManagerClass\" is not defined in the Skyve configuration");
		}
		return result;
	}

	/**
	 * Get a new SQL Data Access instance for the default data store.
	 * Note that this is not a CDI provider as it is auto-closeable.
	 * 
	 * @return A SQLDataAccess.
	 */
	public static @Nonnull SQLDataAccess newSQLDataAccess() {
		return new SQLDataAccessImpl(UtilImpl.DATA_STORE);
	}
	
	/**
	 * Get a new SQL Data Access instance for the given data store.
	 * Note that this is not a CDI provider as it is auto-closeable.
	 * 
	 * @return A SQLDataAccess.
	 */
	public static @Nonnull SQLDataAccess newSQLDataAccess(@Nonnull DataStore dataStore) {
		return new SQLDataAccessImpl(dataStore);
	}
	
	/**
	 * Get a new list model for the given metadata query.
	 * 
	 * @param <T>	The type of model driving document bean implementation 
	 * @param query	The metadata query.
	 * @return	The new model.
	 */
	public static @Nonnull <T extends Bean> ListModel<T> newListModel(@Nonnull MetaDataQueryDefinition query) {
		Customer c = CORE.getCustomer();
		Module m = query.getDocumentModule(c);
		Document d = m.getDocument(c, query.getDocumentName());

		try {
			if (d.isDynamic()) {
				if ((AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS != null) && RDBMSDynamicPersistence.class.isAssignableFrom(AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS)) {
					RDBMSDynamicPersistenceListModel<T> result = new RDBMSDynamicPersistenceListModel<>(query);
					result.postConstruct(c, true);
					return result;
				}
				throw new DomainException("Cannot create new list model for dynamic persistence implementation " + AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS);
			}

			DocumentQueryListModel<T> result = new DocumentQueryListModel<>(query);
			result.postConstruct(c, true);
			return result;
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Throwable t) {
			throw new DomainException("Cannot create new list model", t);
		}
	}
	
	/**
	 * Provide a hash of a clear text password.
	 * 
	 * @param clearText
	 * @return	The encoded password.
	 */
	public static @Nonnull String hashPassword(@Nonnull String clearText) {
		return SecurityUtil.hashPassword(clearText);
	}

	/**
	 * Check a hash against a clear text password.
	 * 
	 * @param	clearText
	 * @param	The encoded password.
	 * @return	true if it matches, or false if it doesn't
	 */
	public static boolean checkPassword(@Nonnull String clearText, @Nonnull String hashedPassword) {
		PasswordEncoder dpe = SecurityUtil.createDelegatingPasswordEncoder();
		return dpe.matches(clearText, hashedPassword);
	}
	
	/**
	 * Inject a bootstrap user according to the settings in the .json Bootstrap stanza
	 * 
	 * @param p	The Persistence to use.
	 * @throws Exception	When something unforeseen occurs.
	 */
	public static void bootstrap(@Nonnull Persistence p) throws Exception {
		User u = p.getUser();
		Customer c = u.getCustomer();
		Module adminMod = c.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document contactDoc = adminMod.getDocument(c, AppConstants.CONTACT_DOCUMENT_NAME);
		Document userDoc = adminMod.getDocument(c, AppConstants.USER_DOCUMENT_NAME);
		Document userRoleDoc = adminMod.getDocument(c, AppConstants.USER_ROLE_DOCUMENT_NAME);
		DocumentQuery q = p.newDocumentQuery(userDoc);
		q.getFilter().addEquals(AppConstants.USER_NAME_ATTRIBUTE_NAME, UtilImpl.BOOTSTRAP_USER);
		PersistentBean user = q.beanResult();
		if (user == null) {
            LOGGER.info("CREATING BOOTSTRAP USER {}/{} ({})",
                    UtilImpl.BOOTSTRAP_CUSTOMER,
                    UtilImpl.BOOTSTRAP_USER,
                    UtilImpl.BOOTSTRAP_EMAIL);

			// Create user
			user = userDoc.newInstance(u);
			u.setId(user.getBizId());
			user.setBizUserId(u.getId());
			BindUtil.set(user, AppConstants.USER_NAME_ATTRIBUTE_NAME, UtilImpl.BOOTSTRAP_USER);
			BindUtil.set(user, AppConstants.PASSWORD_ATTRIBUTE_NAME, u.getPasswordHash());
			BindUtil.set(user, AppConstants.PASSWORD_LAST_CHANGED_ATTRIBUTE_NAME, new DateTime());

			// Create contact
			Bean contact = contactDoc.newInstance(u);
			BindUtil.set(contact, AppConstants.NAME_ATTRIBUTE_NAME, UtilImpl.BOOTSTRAP_USER);
			BindUtil.convertAndSet(contact, AppConstants.CONTACT_TYPE_ATTRIBUTE_NAME, "Person");
			BindUtil.set(contact, AppConstants.EMAIL1_ATTRIBUTE_NAME, UtilImpl.BOOTSTRAP_EMAIL);

			BindUtil.set(user, AppConstants.CONTACT_ATTRIBUTE_NAME, contact);

			// Add roles
			@SuppressWarnings("unchecked")
			List<Bean> roles = (List<Bean>) BindUtil.get(user, AppConstants.ROLES_ATTRIBUTE_NAME);
			if (roles != null) { // should always be
				for (Module m : c.getModules()) {
					String moduleName = m.getName();
					for (Role r : m.getRoles()) {
						Bean role = userRoleDoc.newInstance(u);
						BindUtil.set(role, ChildBean.PARENT_NAME, user);
						BindUtil.set(role, AppConstants.ROLE_NAME_ATTRIBUTE_NAME, String.format("%s.%s", moduleName, r.getName()));
						roles.add(role);
					}
				}
			}
			
			// Save the bootstrap user
			p.save(user);
		}
		else {
            LOGGER.info("BOOTSTRAP USER {}/{} ALREADY EXISTS",
                    UtilImpl.BOOTSTRAP_CUSTOMER,
                    UtilImpl.BOOTSTRAP_USER);
		}
	}

	/**
	 * Get the {@link HttpServletRequest} for the current thread.
	 * <br/>
	 * This method will throw IllegalStateException if there is no request (eg called from a job or other background task).
	 * 
	 * @return The HttpServletRequest
	 */
	public static @Nonnull HttpServletRequest getHttpServletRequest() {
		HttpServletRequestResponse result = WebContainer.getHttpServletRequestResponse();
		if (result == null) {
			throw new IllegalStateException("No request is available");
		}
		return result.getRequest();
	}

	/**
	 * Get the {@link HttpServletResponse} for the current thread.
	 * <br/>
	 * This method will throw IllegalStateException if there is no request (eg called from a job or other background task).
	 * 
	 * @return The HttpServletRequest
	 */
	public static @Nonnull HttpServletResponse getHttpServletRespsone() {
		HttpServletRequestResponse result = WebContainer.getHttpServletRequestResponse();
		if (result == null) {
			throw new IllegalStateException("No response is available");
		}
		return result.getResponse();
	}

	/**
	 * Indicates if the current thread is for a web request.
	 * That is, there are defined {@link getHttpServletRequest} and {@link getHttpServletRespsone}.
	 * Jobs will return false for this call.
	 */
	public static boolean isWebRequest() {
		return (WebContainer.getHttpServletRequestResponse() != null);
	}
	
	/**
	 * Does the given user in given router UX/UI have access to the given UserAccess.
	 * 
	 * @param user The user to test
	 * @param access The user access to test
	 * @param uxui The UX/UI name to test for
	 */
	public static void checkAccess(@Nonnull User user, @Nonnull UserAccess access, @Nonnull String uxui) {
		if (!user.canAccess(access, uxui)) {
			final String userName = user.getName();
			final String moduleName = access.getModuleName();
			final String documentName = access.getDocumentName();
			final String component = access.getComponent();
			final StringBuilder warning = new StringBuilder(256);
			final String resource;
			warning.append("User ").append(userName).append(" cannot access ");
			if (access.isContent()) {
				warning.append("content for document ").append(moduleName).append('.').append(documentName);
				warning.append(" with binding ").append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this content";
			}
			else if (access.isDocumentAggregate()) {
				warning.append("default query for document ").append(moduleName).append('.').append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this query";
			}
			else if (access.isDynamicImage()) {
				warning.append("dynamic image for document ").append(moduleName).append('.').append(documentName);
				warning.append(" with binding ").append(component);
				warning.append(" and UX/UI ").append(uxui);
				resource = "this dynamic image";
			}
			else if (access.isModelAggregate()) {
				warning.append("model for document ").append(moduleName).append('.').append(documentName);
				warning.append(" named ").append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this model";
			}
			else if (access.isPreviousComplete()) {
				warning.append("previous complete for document ").append(moduleName).append('.').append(documentName);
				warning.append(" with binding ").append(component);
				warning.append(" and UX/UI ").append(uxui);
				resource = "this previous data";
			}
			else if (access.isQueryAggregate()) {
				warning.append("query for module ").append(moduleName);
				warning.append(" named ").append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this query";
			}
			else if (access.isReport()) {
				warning.append("report for document ").append(moduleName).append('.').append(documentName);
				warning.append(" named ").append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this report";
			}
			else if (access.isSingular()) {
				warning.append("view for document ").append(moduleName).append('.').append(documentName);
				warning.append(" named ").append(component);
				warning.append(" with UX/UI ").append(uxui);
				resource = "this view";
			}
			else {
				throw new IllegalStateException(access.toString() + " not catered for");
			}

			LOGGER.warn(warning.toString());
			LOGGER.info("If this user already has a document or action privilege, check if they were navigated to this page/resource programatically or by means other than the menu or views and need to be granted access via an <accesses> stanza in the module or view XML.");
			throw new AccessException(resource, userName);
		}
	}
}