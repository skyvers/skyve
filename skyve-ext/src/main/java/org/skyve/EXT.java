package org.skyve;

import java.awt.image.BufferedImage;
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
import javax.websocket.Session;

import org.apache.poi.ss.usermodel.Workbook;
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
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.addin.PF4JAddInManager;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.bizport.POISheet;
import org.skyve.impl.bizport.POIWorkbook;
import org.skyve.impl.bizport.StandardGenerator;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.dataaccess.sql.SQLDataAccessImpl;
import org.skyve.impl.generate.charts.JFreeChartGenerator;
import org.skyve.impl.job.QuartzJobScheduler;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.report.DefaultReporting;
import org.skyve.impl.security.SkyveLegacyPasswordEncoder;
import org.skyve.impl.tag.DefaultTagManager;
import org.skyve.impl.util.MailUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.RDBMSDynamicPersistenceListModel;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.report.Reporting;
import org.skyve.tag.TagManager;
import org.skyve.util.JSON;
import org.skyve.util.Mail;
import org.skyve.util.PushMessage;
import org.skyve.util.Util;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.DelegatingPasswordEncoder;
import org.springframework.security.crypto.password.Pbkdf2PasswordEncoder;
import org.springframework.security.crypto.scrypt.SCryptPasswordEncoder;

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
	 * For Scheduling Jobs, Background Tasks and Reports.
	 * @return A JobScheduler
	 */
	public static JobScheduler getJobScheduler() {
		return QuartzJobScheduler.get();
	}
	
	/**
	 * For tag operations.
	 * @return A tag manager
	 */
	public static TagManager getTagManager() {
		return DefaultTagManager.get();
	}
	
	/**
	 * Get a reporting service.
	 * @return	A reporting service.
	 */
	public static Reporting getReporting() {
		return DefaultReporting.get();
	}
	
	/**
	 * Get a cache manager
	 * @ return A cache manager
	 */
	public static Caching getCaching() {
		return DefaultCaching.get();
	}
	
	/**
	 * Get an add-in manager
	 * @ return An add-in manager
	 */
	public static AddInManager getAddInManager() {
		return PF4JAddInManager.get();
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
		final ContentManager result = (AbstractContentManager.IMPLEMENTATION_CLASS == null) ?
										PF4JAddInManager.get().getExtension(ContentManager.class) :
										AbstractContentManager.get();
		if (result == null) {
			throw new DomainException("No content manager addin detected and \"factories.contentManagerClass\" is not defined in the Skyve configuration");
		}
		return result;
	}

	// NB Not a CDI provider as it is auto-closeable 
	public static SQLDataAccess newSQLDataAccess() {
		return new SQLDataAccessImpl(UtilImpl.DATA_STORE);
	}
	
	public static SQLDataAccess newSQLDataAccess(DataStore dataStore) {
		return new SQLDataAccessImpl(dataStore);
	}
	
	public static <T extends Bean> ListModel<T> newListModel(MetaDataQueryDefinition query) {
		Customer c = CORE.getCustomer();
		Module m = query.getDocumentModule(c);
		Document d = m.getDocument(c, query.getDocumentName());

		try {
			if (d.isDynamic()) {
				if ((AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS != null) && RDBMSDynamicPersistence.class.isAssignableFrom(AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS)) {
					RDBMSDynamicPersistenceListModel<T> result = new RDBMSDynamicPersistenceListModel<>();
					result.setQuery(query);
					return result;
				}
				throw new DomainException("Cannot create new list model for dynamic persistence implementation " + AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS);
			}

			DocumentQueryListModel<T> result = new DocumentQueryListModel<>();
			result.setQuery(query);
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
		Module adminMod = c.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document contactDoc = adminMod.getDocument(c, AppConstants.CONTACT_DOCUMENT_NAME);
		Document userDoc = adminMod.getDocument(c, AppConstants.USER_DOCUMENT_NAME);
		Document userRoleDoc = adminMod.getDocument(c, AppConstants.USER_ROLE_DOCUMENT_NAME);
		DocumentQuery q = p.newDocumentQuery(userDoc);
		q.getFilter().addEquals(AppConstants.USER_NAME_ATTRIBUTE_NAME, UtilImpl.BOOTSTRAP_USER);
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
			for (Module m : c.getModules()) {
				String moduleName = m.getName();
				for (Role r : m.getRoles()) {
					Bean role = userRoleDoc.newInstance(u);
					BindUtil.set(role, ChildBean.PARENT_NAME, user);
					BindUtil.set(role, AppConstants.ROLE_NAME_ATTRIBUTE_NAME, String.format("%s.%s", moduleName, r.getName()));
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
