package org.skyve.wildcat.persistence.hibernate;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.logging.Level;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityNotFoundException;
import javax.persistence.EntityTransaction;
import javax.persistence.RollbackException;
import javax.sql.DataSource;

import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.hibernate.EntityMode;
import org.hibernate.Filter;
import org.hibernate.FlushMode;
import org.hibernate.HibernateException;
import org.hibernate.LockMode;
import org.hibernate.Query;
import org.hibernate.SQLQuery;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.StaleObjectStateException;
import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernate.dialect.Dialect;
import org.hibernate.dialect.SQLServerDialect;
import org.hibernate.ejb.Ejb3Configuration;
import org.hibernate.ejb.HibernateEntityManager;
import org.hibernate.ejb.HibernateEntityManagerFactory;
import org.hibernate.ejb.event.EJB3FlushEntityEventListener;
import org.hibernate.ejb.event.EJB3PostInsertEventListener;
import org.hibernate.ejb.event.EJB3PostUpdateEventListener;
import org.hibernate.engine.Mapping;
import org.hibernate.event.FlushEntityEventListener;
import org.hibernate.event.InitializeCollectionEventListener;
import org.hibernate.event.PostInsertEventListener;
import org.hibernate.event.PostUpdateEventListener;
import org.hibernate.event.PreUpdateEventListener;
import org.hibernate.event.def.DefaultInitializeCollectionEventListener;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.Index;
import org.hibernate.mapping.Table;
import org.hibernate.tool.hbm2ddl.ColumnMetadata;
import org.hibernate.tool.hbm2ddl.DatabaseMetadata;
import org.hibernate.tool.hbm2ddl.TableMetadata;
import org.hibernate.type.Type;
import org.hibernate.util.ArrayHelper;
import org.hibernatespatial.AbstractDBGeometryType;
import org.hibernatespatial.SpatialDialect;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.messages.OptimisticLockException.OperationType;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.SQL;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.content.BeanContent;
import org.skyve.wildcat.domain.AbstractPersistentBean;
import org.skyve.wildcat.domain.messages.ReferentialConstraintViolationException;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.metadata.customer.CustomerImpl.ExportedReference;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.metadata.model.document.field.Enumeration;
import org.skyve.wildcat.metadata.model.document.field.Field;
import org.skyve.wildcat.metadata.model.document.field.Field.IndexType;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.metadata.user.UserImpl;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.BeanVisitor;
import org.skyve.wildcat.util.CascadeDeleteBeanVisitor;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.ValidationUtil;

public abstract class AbstractHibernatePersistence extends AbstractPersistence {
	private static final long serialVersionUID = -1813679859498468849L;

	private static final String DIALECT_PACKAGE = "org.hibernate.dialect.";
	private static EntityManagerFactory emf = null;
	private static AbstractDBGeometryType geometryUserType = null;

	static {
		try {
			configure();
		}
		catch (MetaDataException e) {
			throw new IllegalStateException("Cannot initialize persistence", e);
		}
	}

	private EntityManager em = null;
	private Session session = null;

	public AbstractHibernatePersistence() {
		em = emf.createEntityManager();
		session = ((HibernateEntityManager) em).getSession();
		session.setFlushMode(FlushMode.MANUAL);
	}

	protected abstract void removeBeanContent(PersistentBean bean) throws Exception;
	protected abstract void putBeanContent(BeanContent content) throws Exception;
	protected abstract void moveBeanContent(BeanContent content, String oldBizDataGroupId, String oldBizUserId) throws Exception;
	protected abstract void removeAttachmentContent(String contentId) throws Exception;
	protected abstract void commitContent() throws Exception;
	
	@Override
	public final void disposeAllPersistenceInstances() 
	throws MetaDataException {
		// remove this instance - and hopefully the only instance running
		commit(true);

		emf.close();
		emf = null;

		if (UtilImpl.WILDCAT_PERSISTENCE_CLASS == null) {
			AbstractPersistence.IMPLEMENTATION_CLASS = HibernateElasticSearchPersistence.class;
		}
		else {
			try {
				AbstractPersistence.IMPLEMENTATION_CLASS = (Class<? extends AbstractPersistence>) Class.forName(UtilImpl.WILDCAT_PERSISTENCE_CLASS);
			}
			catch (ClassNotFoundException e) {
				throw new IllegalStateException("Could not find WILDCAT_PERSISTENCE_CLASS " + UtilImpl.WILDCAT_PERSISTENCE_CLASS, e);
			}
		}

		configure();
	}

	private static void configure() 
	throws MetaDataException {
		Ejb3Configuration cfg = new Ejb3Configuration();

		if (UtilImpl.DATASOURCE == null) {
			cfg.setProperty("hibernate.connection.driver_class", UtilImpl.STANDALONE_DATABASE_JDBC_DRIVER);
			cfg.setProperty("hibernate.connection.url", UtilImpl.STANDALONE_DATABASE_CONNECTION_URL);
			cfg.setProperty("hibernate.connection.username", UtilImpl.STANDALONE_DATABASE_USERNAME);
			cfg.setProperty("hibernate.connection.password", UtilImpl.STANDALONE_DATABASE_PASSWORD);
			cfg.setProperty("hibernate.connection.autocommit", "false");
		}
		else {
			cfg.setProperty("hibernate.connection.datasource", UtilImpl.DATASOURCE);
		}

		if (UtilImpl.DIALECT.indexOf('.') < 0) {
			cfg.setProperty("hibernate.dialect", DIALECT_PACKAGE + UtilImpl.DIALECT);
		}
		else {
			cfg.setProperty("hibernate.dialect", UtilImpl.DIALECT);
		}

		// Query Caching screws up pessimistic locking
		cfg.setProperty("hibernate.cache.use_query_cache", "false");

		HibernateListener hibernateListener = new HibernateListener();
		
		// For CMS Update callbacks
		cfg.setListeners("post-update", new PostUpdateEventListener[] {new EJB3PostUpdateEventListener(), hibernateListener});
		cfg.setListeners("post-insert",  new PostInsertEventListener[] {new EJB3PostInsertEventListener(), hibernateListener});

		// For BizLock and BizKey callbacks
		cfg.setListeners("pre-update", new PreUpdateEventListener[] {hibernateListener});

		// For ordering collection elements when initialised
		cfg.setListeners("load-collection", new InitializeCollectionEventListener[] {new DefaultInitializeCollectionEventListener(), hibernateListener});

		// For flush callbacks
		cfg.setListeners("flush-entity", new FlushEntityEventListener[] {hibernateListener, new EJB3FlushEntityEventListener()});
		
		// For collection mutation callbacks
		// NB this didn't work - got the event name from the hibernate envers doco - maybe in a new version of hibernate
//		cfg.setListeners("pre-collection-update", new PreCollectionUpdateEventListener[] {hibernateListener});
//		cfg.setListeners("pre-collection-remove", new PreCollectionRemoveEventListener[] {hibernateListener});
		
		// JDBC parameters
		cfg.setProperty("hibernate.jdbc.use_streams_for_binary", "true");
		cfg.setProperty("hibernate.jdbc.batch_size", "16");
		cfg.setProperty("hibernate.max_fetch_depth", "3");

		// Whether to generate dynamic proxies as classes or not (adds to classes loaded and thus Permanent Generation)
		cfg.setProperty("hibernate.bytecode.use_reflection_optimizer", "false");

		// Update the database schema on first use
		if (UtilImpl.DDL_SYNC) {
			cfg.setProperty("hibernate.hbm2ddl.auto", "update");
		}

		// Keep stats on usage
		cfg.setProperty("hibernate.generate_statistics", "false");

		// Log SQL to stdout
		cfg.setProperty("hibernate.show_sql", Boolean.toString(UtilImpl.SQL_TRACE));
		cfg.setProperty("hibernate.format_sql", Boolean.toString(UtilImpl.PRETTY_SQL_OUTPUT));

		// Don't import simple class names as entity names
		cfg.setProperty("auto-import", "false");

		cfg.addAnnotatedClass(AbstractPersistentBean.class);

		AbstractRepository repository = AbstractRepository.get();

		if (UtilImpl.USING_JPA) {
			cfg.configure("bizhub", null);
			// emf = javax.persistence.Persistence.createEntityManagerFactory("bizhub");
		}
		else {
			StringBuilder sb = new StringBuilder(64);

			for (String moduleName : repository.getAllVanillaModuleNames()) {
				// repository.REPOSITORY_DIRECTORY
				sb.setLength(0);
				sb.append(repository.MODULES_NAME).append('/');
				sb.append(moduleName).append('/');
				sb.append(repository.DOMAIN_NAME).append('/');
				sb.append(moduleName).append("_orm.hbm.xml");
				String mappingPath = sb.toString();

				File mappingFile = new File(UtilImpl.getAbsoluteBasePath() + mappingPath);
				if (mappingFile.exists()) {
					cfg.addResource(mappingPath);
				}
			}

			// Check for customer overridden ORMs
			for (String customerName : repository.getAllCustomerNames()) {
				sb.setLength(0);
				sb.append(repository.CUSTOMERS_NAMESPACE).append(customerName).append('/');
				sb.append(repository.MODULES_NAME).append("/orm.hbm.xml");
				String ormResourcePath = sb.toString();
				
				File ormFile = new File(UtilImpl.getAbsoluteBasePath() + ormResourcePath);
				if (ormFile.exists()) {
					cfg.addResource(ormResourcePath);
				}
			}
		}

		emf = cfg.buildEntityManagerFactory();
		if (UtilImpl.DDL_SYNC) {
			try {
				generateExtraSchemaUpdates(cfg.getHibernateConfiguration(), true);
			}
			catch (Exception e) {
				throw new MetaDataException("Could not apply WILDCAT extra schema updates", e);
			}
		}
	}

	static AbstractDBGeometryType getGeometryUserType() throws Exception {
		if (geometryUserType == null) {
			SpatialDialect dialect = (SpatialDialect) Class.forName(UtilImpl.DIALECT).newInstance();
			geometryUserType = (AbstractDBGeometryType) dialect.getGeometryUserType();
		}
		
		return geometryUserType;
	}
	
	@SuppressWarnings("resource")
	private static String[] generateExtraSchemaUpdates(Configuration cfg, boolean doUpdate)
	throws SQLException, NamingException, ClassNotFoundException {
		UtilImpl.LOGGER.info("Apply WILDCAT extra schema updates");
        ArrayList<String> result = new ArrayList<>(50);

        Connection connection = null;
		if (UtilImpl.DATASOURCE == null) {
			Class.forName(UtilImpl.STANDALONE_DATABASE_JDBC_DRIVER);
			Properties connectionProps = new Properties();
			connectionProps.put("user", UtilImpl.STANDALONE_DATABASE_USERNAME);
			connectionProps.put("password", UtilImpl.STANDALONE_DATABASE_PASSWORD);
			connection = DriverManager.getConnection(UtilImpl.STANDALONE_DATABASE_CONNECTION_URL,
														connectionProps);
		}
		else {
	        DataSource dataSource = (DataSource) new InitialContext().lookup(UtilImpl.DATASOURCE);
			connection = dataSource.getConnection();
		}

		if (connection == null) {
			throw new IllegalStateException("Connection is null");
		}

		try {
	        try (Statement statement = connection.createStatement()) {
	            Dialect dialect = Dialect.getDialect(cfg.getProperties());
	            DatabaseMetadata meta = new DatabaseMetadata( connection, dialect );
	            Mapping mapping = cfg.buildMapping();
	            
	            Properties properties = cfg.getProperties();
	            String defaultCatalog = properties.getProperty(Environment.DEFAULT_CATALOG);
	            String defaultSchema = properties.getProperty(Environment.DEFAULT_SCHEMA);

	            Iterator<?> iter = cfg.getTableMappings();
	            while (iter.hasNext()) {
	                Table table = (Table) iter.next();
	                if (table.isPhysicalTable()) {
	                    TableMetadata tableInfo = meta.getTableMetadata(
	                            table.getName(),
	                            (table.getSchema() == null) ? defaultSchema : table.getSchema(),
	                            (table.getCatalog() == null) ? defaultCatalog : table.getCatalog(),
	                            table.isQuoted());
	                    if (tableInfo != null) { // already exists - does it need altering?
	                        if (dialect.hasAlterTable()) { // can we alter it in this database?
                        		String ddl = null;
	                        	try {
	                        		Iterator<?> subIter = sqlExtraAlterStrings(table,
	        	                                                                dialect,
	        	                                                                mapping,
	        	                                                                tableInfo,
	        	                                                                defaultCatalog,
	        	                                                                defaultSchema);
		                            while (subIter.hasNext()) {
		                                ddl = (String) subIter.next();
		                                if (doUpdate) {
		                            		UtilImpl.LOGGER.info(ddl);
		                                    statement.executeUpdate(ddl);
		                                }
		                                result.add(ddl);
		                            }
		                            
		                            subIter = table.getIndexIterator();
		                            while (subIter.hasNext()) {
		                                Index index = (Index) subIter.next();
		                                if (tableInfo.getIndexMetadata(index.getName()) == null) {
		                                    ddl = index.sqlCreateString(dialect,
	                                                                        mapping,
	                                                                        defaultCatalog,
	                                                                        defaultSchema);
		                                    if (doUpdate) {
		                                		UtilImpl.LOGGER.info(ddl);
		                                        statement.executeUpdate(ddl);
		                                    }
		                                    result.add(ddl);
		                                }
		                            }
	                        	}
	                        	catch (SQLException e) {
	                    			UtilImpl.LOGGER.severe("Could not apply WILDCAT extra schema updates : " + ddl);
	                        	}
	                        }
	                    }
	                }
	            }
	        }
	    }
	    finally {
    		connection.close();
	    }
	    
	    return ArrayHelper.toStringArray(result);
	}

    /**
     * This method exists because by default, hibernate does not issue "alter table modify column" statements only
     * "alter table add column" statements. So this hack allows alter table modify/alter column when the type, length or precision
     * has changed. The "modify column" syntax is outside the scope of the hibernate dialect classes so I;ve had to hack/hard-code
     * it.
     */
    private static Iterator<String> sqlExtraAlterStrings(Table table,
		                                                    Dialect dialect,
		                                                    Mapping mapping,
		                                                    TableMetadata tableInfo,
		                                                    String defaultCatalog,
		                                                    String defaultSchema)
    throws HibernateException {
        StringBuffer root = new StringBuffer("alter table ").append(table.getQualifiedName(dialect, defaultCatalog, defaultSchema)).append(' ');

        Iterator<?> iter = table.getColumnIterator();
        List<String> results = new ArrayList<>();
        while (iter.hasNext()) {
            Column column = (Column) iter.next();
            ColumnMetadata columnInfo = tableInfo.getColumnMetadata(column.getName());

            int sqlTypeCode = column.getSqlTypeCode(mapping);
            if ((columnInfo != null) && // column exists
                    // char column and lengths are different
                    ((((sqlTypeCode == Types.VARCHAR) || 
                            (sqlTypeCode == Types.CHAR) || 
                            (sqlTypeCode == Types.LONGVARCHAR) || 
                            (sqlTypeCode == Types.LONGNVARCHAR)) && 
                        (column.getLength() != columnInfo.getColumnSize())) ||
	                    // decimal column and scales are different
	                    (((sqlTypeCode == Types.FLOAT) || 
	                            (sqlTypeCode == Types.REAL) || 
	                            (sqlTypeCode == Types.DOUBLE) || 
	                            (sqlTypeCode == Types.NUMERIC)) && 
	                        ((column.getScale() != columnInfo.getDecimalDigits()) || 
	                            (column.getPrecision() != columnInfo.getColumnSize()))))) {
            	StringBuffer alter = new StringBuffer(root.toString())
                                .append((dialect instanceof SQLServerDialect) ? "alter column" : "modify column")
                                .append(' ').append(column.getQuotedName(dialect)).append(' ')
                                .append(column.getSqlType(dialect, mapping));

                String defaultValue = column.getDefaultValue();
                if (defaultValue != null) {
                    alter.append(" default ").append(defaultValue);

                    if (column.isNullable()) {
                        alter.append(dialect.getNullColumnString());
                    }
                    else {
                        alter.append(" not null");
                    }
                }

                boolean useUniqueConstraint = column.isUnique() && dialect.supportsUnique() &&
                                (! column.isNullable() || dialect.supportsNotNullUnique());
                if (useUniqueConstraint) {
                    alter.append(" unique");
                }

                if (column.hasCheckConstraint() && dialect.supportsColumnCheck()) {
                    alter.append(" check(").append(column.getCheckConstraint()).append(")");
                }

                String columnComment = column.getComment();
                if (columnComment != null) {
                    alter.append(dialect.getColumnComment(columnComment));
                }

                results.add(alter.toString());
            }
        }

        return results.iterator();
    }
	
	@Override
	public final String generateDDL()
	throws DomainException, MetaDataException {
		Properties properties = new Properties();
		properties.put("hibernate.connection.datasource", UtilImpl.DATASOURCE);
		properties.put("hibernate.dialect", DIALECT_PACKAGE + UtilImpl.DIALECT);

		Configuration cfg = new Configuration();
		cfg.setProperties(properties);

		AbstractRepository repository = AbstractRepository.get();
		ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

		StringBuilder sb = new StringBuilder(256);
		for (String moduleName : repository.getAllVanillaModuleNames()) {
			sb.setLength(0);
			sb.append(repository.MODULES_NAME).append('/');
			sb.append(moduleName).append('/');
			sb.append(repository.DOMAIN_NAME).append('/');
			sb.append(moduleName).append("_orm.hbm.xml");
			cfg.addResource(sb.toString(), classLoader);
		}

		Dialect dialect = Dialect.getDialect(properties);

		String[] drops = cfg.generateDropSchemaScript(dialect);
		String[] creates = cfg.generateSchemaCreationScript(dialect);

		DatabaseMetadata meta;
		try (Connection connection = getConnection()) {
			meta = new DatabaseMetadata(connection, dialect);

			String[] updates = cfg.generateSchemaUpdateScript(dialect, meta);
			sb.setLength(0);
			sb.append("\n********* DROPS *********\n\n");
			for (String drop : drops) {
				sb.append(drop).append(";\n");
			}
			sb.append("\n******** CREATES ********\n\n");
			for (String create : creates) {
				sb.append(create).append(";\n");
			}
			sb.append("\n******** UPDATES ********\n\n");
			for (String update : updates) {
				sb.append(update).append(";\n");
			}
            sb.append("\n***** EXTRA UPDATES *****\n\n");
			updates = generateExtraSchemaUpdates(cfg, false);
            for (String update : updates) {
                sb.append(update).append(";\n");
            }
		}
		catch (Exception e) {
			throw new DomainException("Could not get database metadata", e);
		}

		return sb.toString();
	}

	@Override
	public final String getDocumentEntityName(String moduleName, String documentName) {
		SessionFactory sf = ((HibernateEntityManagerFactory) emf).getSessionFactory();
		String overriddenEntityName = user.getCustomerName() + moduleName + documentName;

		if (sf.getClassMetadata(overriddenEntityName) != null) {
			return overriddenEntityName;
		}

		return moduleName + documentName;
	}

	private void treatPersistenceThrowable(Throwable t, OperationType operationType, OptimisticLock persistentLock) 
	throws DomainException, MetaDataException {
t.printStackTrace();
		if (t instanceof javax.persistence.OptimisticLockException) {
			throw new OptimisticLockException(user.getCustomer(), operationType, persistentLock);
		}
		else if (t instanceof StaleObjectStateException) {
			throw new OptimisticLockException(user.getCustomer(), operationType, persistentLock);
		}
		else if (t instanceof EntityNotFoundException) {
			throw new OptimisticLockException(user.getCustomer(), operationType, persistentLock);
		}
		else if (t instanceof DomainException) {
			throw (DomainException) t;
		}
		else if (t instanceof MetaDataException) {
			throw (MetaDataException) t;
		}
		else if (t.getCause() instanceof DomainException) {
			throw (DomainException) t.getCause();
		}
		else if (t.getCause() instanceof MetaDataException) {
			throw (MetaDataException) t.getCause();
		}
		else {
			throw new DomainException(t);
		}
	}

	@Override
	public final void begin() {
		EntityTransaction et = em.getTransaction();
		if (! et.isActive()) {
			// FROM THE HIBERNATE_REFERENCE DOCS Page 190
            // Earlier versions of Hibernate required explicit disconnection and reconnection of a Session. 
            // These methods are deprecated, as beginning and ending a transaction has the same effect.
			et.begin();
		}
	}

	@Override
	public void setUser(User user) throws MetaDataException {
		super.setUser(user);
		resetDocumentPermissionScopes();
	}

	@Override
	public void setDocumentPermissionScopes(DocumentPermissionScope scope)
	throws MetaDataException {
		Set<String> accessibleModuleNames = ((UserImpl) user).getAccessibleModuleNames(); 
		AbstractRepository repository = AbstractRepository.get();

		// Enable all filters required for this user
		for (String moduleName : repository.getAllVanillaModuleNames()) {
			Customer moduleCustomer = (accessibleModuleNames.contains(moduleName) ? user.getCustomer() : null);
			Module module = repository.getModule(moduleCustomer, moduleName);

			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(moduleCustomer, documentName);
				Persistent persistent = document.getPersistent();
				if ((persistent != null) &&  // is persistent document
						(persistent.getName() != null) && // and has a persistent name
						(repository.findNearestPersistentUnmappedSuperDocument(moduleCustomer, module, document) == null) && // not a sub-class (which don't have filters)
						moduleName.equals(document.getOwningModuleName())) { // document belongs to this module
					setFilters(document, scope);
				}
			}
		}
	}

	@Override
	public void resetDocumentPermissionScopes() throws MetaDataException {
		Set<String> accessibleModuleNames = ((UserImpl) user).getAccessibleModuleNames(); 
		AbstractRepository repository = AbstractRepository.get();

//		String userDataGroupId = user.getDataGroupId();
//		if (Util.SECURITY_TRACE) {
//			Util.LOGGER.info("SET USER: cust=" + customer.getName() + " datagroup=" + userDataGroupId + " user=" + user.getId());
//		}
		
		// Enable all filters required for this user
		for (String moduleName : repository.getAllVanillaModuleNames()) {
			Customer moduleCustomer = (accessibleModuleNames.contains(moduleName) ? user.getCustomer() : null);
			Module module = repository.getModule(moduleCustomer, moduleName);

			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(moduleCustomer, documentName);
				Persistent persistent = document.getPersistent();
				if ((persistent != null) &&  // is persistent document
						(persistent.getName() != null) && // with a persistent name
						(repository.findNearestPersistentUnmappedSuperDocument(moduleCustomer, module, document) == null) && // not a sub-class (which don't have filters)
						moduleName.equals(document.getOwningModuleName())) { // document belongs to this module
					resetFilters(document);
				}
			}
		}
	}

	/**
	 * Setup the session filters for the scope given.
	 * 
	 * @param document
	 * @param newScope
	 * @return
	 */
	private void setFilters(Document document, DocumentPermissionScope scope)
	throws MetaDataException {
		Set<String> accessibleModuleNames = ((UserImpl) user).getAccessibleModuleNames(); 
		AbstractRepository repository = AbstractRepository.get();
		String userDataGroupId = user.getDataGroupId();
		Customer customer = user.getCustomer();
		String moduleName = document.getOwningModuleName();
		
		Document filterDocument = document;
		Document tempFilterDocument = document;

		while (tempFilterDocument != null) {
			Customer moduleCustomer = (accessibleModuleNames.contains(moduleName) ? customer : null);
			Module module = repository.getModule(moduleCustomer, moduleName);

			tempFilterDocument = repository.findNearestPersistentUnmappedSuperDocument(moduleCustomer, 
																						module,
																						tempFilterDocument);
			if (tempFilterDocument != null) {
				filterDocument = tempFilterDocument;
			}
		}

		String entityName = getDocumentEntityName(filterDocument.getOwningModuleName(), filterDocument.getName());
		
		String noneFilterName = new StringBuilder(32).append(entityName).append("NoneFilter").toString();
		String customerFilterName = new StringBuilder(32).append(entityName).append("CustomerFilter").toString();
		String dataGroupIdFilterName = new StringBuilder(32).append(entityName).append("DataGroupIdFilter").toString();
		String userIdFilterName = new StringBuilder(32).append(entityName).append("UserIdFilter").toString();
		session.disableFilter(noneFilterName);
		session.disableFilter(customerFilterName);
		session.disableFilter(dataGroupIdFilterName);
		session.disableFilter(userIdFilterName);
		
		if (DocumentPermissionScope.none.equals(scope)) {
			session.enableFilter(noneFilterName);
		}
		if (DocumentPermissionScope.customer.equals(scope) ||
				DocumentPermissionScope.dataGroup.equals(scope) ||
				DocumentPermissionScope.user.equals(scope)) {
			Filter filter = session.enableFilter(customerFilterName);
			filter.setParameter("customerParam", customer.getName());
		}
		if ((userDataGroupId != null) && 
				(DocumentPermissionScope.dataGroup.equals(scope) ||
					DocumentPermissionScope.user.equals(scope))) {
			Filter filter = session.enableFilter(dataGroupIdFilterName);
			filter.setParameter("dataGroupIdParam", userDataGroupId);
		}
		if (DocumentPermissionScope.user.equals(scope)) {
			Filter filter = session.enableFilter(userIdFilterName);
			filter.setParameter("userIdParam", user.getId());
		}
	}
	
	/**
	 * Reset filters to the document default
	 * 
	 * @param document
	 * @param scope
	 */
	private void resetFilters(Document document)
	throws MetaDataException {
		DocumentPermissionScope scope = user.getScope(document.getOwningModuleName(), document.getName());
		setFilters(document, scope);
	}
	
	// This code is called in exception blocks all over the place.
	// So we have to ensure its robust as all fuck
	@Override
	public final void rollback() {
		if (em != null) {
			EntityTransaction et = em.getTransaction();
			if ((et != null) && et.isActive()) {
                // FROM THE HIBERNATE_REFERENCE DOCS Page 190
                // Earlier versions of Hibernate required explicit disconnection and reconnection of a Session. 
                // These methods are deprecated, as beginning and ending a transaction has the same effect.
				et.rollback();
			}
		}
	}

	// This code is called in finally blocks all over the place.
	// So we have to ensure its robust as all fuck
	@Override
	public final void commit(boolean close) {
		try {
			if (em != null) { // can happen after a relogin
				EntityTransaction et = em.getTransaction();
				if ((et != null) && et.isActive()) {
				    // FROM THE HIBERNATE_REFERENCE DOCS Page 190
				    // Earlier versions of Hibernate required explicit disconnection and reconnection of a Session. 
				    // These methods are deprecated, as beginning and ending a transaction has the same effect.
				    et.commit();
				}
			}
		}
		catch (RollbackException e) {
			UtilImpl.LOGGER.warning("Cannot commit as transaction was rolled back earlier....");
		}
		finally {
			try {
				commitContent();
			}
			catch (Exception e) {
				UtilImpl.LOGGER.warning("Cannot commit content manager - " + e.getLocalizedMessage());
				e.printStackTrace();
			}
			finally {
				if (close) {
					if (em != null) { // can happen after a relogin
						em.close();
					}
					em = null;
					session = null;
					threadLocalPersistence.remove();
				}
			}
		}
	}

	@Override
	public void evictAllCached() {
		session.clear();
	}

	@Override
	public void evictCached(Bean bean) {
		session.evict(bean);
	}

	// populate all implicit mandatory fields required
	private void setMandatories(Document document, final Bean beanToSave)
	throws DomainException, MetaDataException {
		final Customer customer = user.getCustomer();

		new BeanVisitor(false, false, false) {
			@Override
			@SuppressWarnings("synthetic-access")
			protected boolean accept(String binding,
										@SuppressWarnings("hiding") Document document,
										Document parentDocument,
										Relation parentRelation,
										Bean bean) 
			throws Exception {
				Persistent persistent = document.getPersistent();
				if ((persistent != null) && 
						(persistent.getName() != null)) { // persistent document
					// dataGroup and user are NOT set here - it is only set on newInstance()
					// this allows us to set the data group programmatically.
					// DataGroup and user are used to create a path
					// for content and so can not change.
					// 
					// Content should be saved in the 1 workspace, this means bizCustomer is not sych a massive security risk.
					// Customer is not the broadest scope - global is, so customer data can be interrelated at the customer level.
					// ie bizhub could maintain global post codes for all its customers, who may link to it, but when the other
					// customers save their data, we do not want them taking ownership of our postcodes.
//					bean.setBizCustomer(customer.getName());

					// We set the bizKey unconditionally as the bizKey may be dependent on child or related
					// beans and not just on properties in this bean.
					// That means we can't rely on preUpdate event listener as preUpdate may not get fired if this 
					// bean hasn't changed, but the related dependent bean has changed requiring the update to bizKey.
					PersistentBean persistentBean = (PersistentBean) bean;
					persistentBean.setBizKey(Util.processStringValue(persistentBean.getBizKey()));

					// This only sets the bizLock if the bean is about to be inserted
					// If we set it here on update, we are making the bean dirty even if there
					// are no actual changes made to it.
					// The HibernateListener is used to generate bizLock on update.
					// Note:- This stops hibernate metadata validation from moaning
					if (! bean.isPersisted()) {
						persistentBean.setBizLock(new OptimisticLock(user.getName(), new Date()));
					}
				}

				return true;
			}
		}.visit(document, beanToSave, customer);
	}
	
	@Override
	public void preFlush(Document document, final Bean beanToSave)
	throws DomainException, MetaDataException {
		// set bizCustomer, bizLock & bizKey
		setMandatories(document, beanToSave);
		
		// Validate all and sundry before touching the database
		final Customer customer = user.getCustomer();
		new BeanVisitor(false, false, false) {
			@Override
			@SuppressWarnings( {"synthetic-access"})
			protected boolean accept(String binding,
										@SuppressWarnings("hiding") Document document,
										Document parentDocument,
										Relation parentRelation,
										Bean bean)
			throws Exception {
				// NOTE:- We only check if the document is a persistent document here,
				// not if the reference (if any) is persistent.
				// We could have a transient reference to a persistent document and 
				// the save operation still needs to cascade persist any changes to the 
				// persistent attributes in the referenced document.
				Persistent persistent = document.getPersistent();
				String persistentName = (persistent == null) ? null : persistent.getName();
				try {
					Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);

					// persistent
					if (persistentName != null) {
						CustomerImpl internalCustomer = (CustomerImpl) customer;
						boolean vetoed = internalCustomer.interceptBeforePreSave(bean);
						if (! vetoed) {
							if (bizlet != null) {
								if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preSave", "Entering " + bizlet.getClass().getName() + ".preSave: " + bean);
								bizlet.preSave(bean);
								if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preSave", "Exiting " + bizlet.getClass().getName() + ".preSave");
							}
							internalCustomer.interceptAfterPreSave(bean);
						}
					}
					
					ValidationUtil.validateBeanAgainstDocument(document, bean);

					if (bizlet != null) { // has a bizlet
						ValidationUtil.validateBeanAgainstBizlet(bizlet, bean);
					}

					ValidationUtil.checkCollectionUniqueConstraints(customer, document, bean);

					if (persistentName != null) { // persistent
						checkUniqueConstraints(document, bean);

						// Re-evaluate the bizKey after all events have fired
						// as the bizKey may be dependent on values that have mutated  
						// NB - We do unconditionally as the bizKey may be dependent on child or related
						// beans and not just on properties in this bean.
						// That means we can't rely on preUpdate event listener as preUpdate may not get fired if this 
						// bean hasn't changed, but the related dependent bean has changed requiring the update to bizKey.
						PersistentBean persistentBean = (PersistentBean) bean;
						persistentBean.setBizKey(Util.processStringValue(persistentBean.getBizKey()));
					}
				}
				catch (ValidationException e) {
					for (Message message : e.getMessages()) {
						ValidationUtil.processErrorMessageBindings(customer, message, beanToSave, bean);
					}
					throw e;
				}

				return true;
			}
		}.visit(document, beanToSave, customer);

		// set bizCustomer, bizLock & bizKey again in case 
		// more object hierarchy has been added during preSave()
		setMandatories(document, beanToSave);
	}

	@Override
	@SuppressWarnings("unchecked")
	public final <T extends PersistentBean> T save(Document document, T bean)
	throws DomainException, MetaDataException {
		T result = null;
		
		try {
			CustomerImpl internalCustomer = (CustomerImpl) getUser().getCustomer();
			boolean vetoed = false;
			
			// We need to replace transient properties before calling postFlush as
			// Bizlet.postSave() implementations could manipulate these transients for display after save.
			try {
				vetoed = internalCustomer.interceptBeforeSave(document, bean);
				if (! vetoed) {
					preFlush(document, bean);
					String entityName = getDocumentEntityName(document.getOwningModuleName(), document.getName());
					result = (T) session.merge(entityName, bean);
					em.flush();
				}
			}
			finally {
				if (result != null) { // only do if we got a result from the merge
					replaceTransientProperties(document, result, bean);
				}
			}
			if (! vetoed) {
				postFlush(document, result);
				internalCustomer.interceptAfterSave(document, result);
			}
		}
		catch (Throwable t) {
			treatPersistenceThrowable(t, OperationType.update, bean.getBizLock());
		}

		return result;
	}

	@Override
	public void postFlush(Document document, final Bean beanToSave)
	throws DomainException, MetaDataException {
		final Customer customer = user.getCustomer();
		
		new BeanVisitor(false, false, false) {
			@Override
			protected boolean accept(String binding,
										@SuppressWarnings("hiding") Document document,
										Document parentDocument,
										Relation parentRelation,
										Bean bean) 
			throws Exception {
				Persistent persistent = document.getPersistent();
				String persistentName = (persistent == null) ? null : persistent.getName();
				
				// persistent for this bean graph
				if (persistentName != null) {
					try {
						CustomerImpl internalCustomer = (CustomerImpl) customer;
						boolean vetoed = internalCustomer.interceptBeforePostSave(bean);
						if (! vetoed) {
							Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
							if (bizlet != null) {
								if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "postSave", "Entering " + bizlet.getClass().getName() + ".postSave: " + bean);
								bizlet.postSave(bean);
								if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "postSave", "Exiting " + bizlet.getClass().getName() + ".postSave");
							}
							internalCustomer.interceptAfterPostSave(bean);
						}
					}
					catch (ValidationException e) {
						for (Message message : e.getMessages()) {
							ValidationUtil.processErrorMessageBindings(customer, message, beanToSave, bean);
						}
						throw e;
					}
				}

				// now that we have saved, clear the values
				bean.originalValues().clear();

				return true;
			}
		}.visit(document, beanToSave, customer);
	}

	@Override
	public void replaceTransientProperties(Document document, final Bean savedBean, final Bean unmergedBean)
	throws DomainException, MetaDataException {
		Customer customer = user.getCustomer();

		new BeanVisitor(false, false, false) {
			@Override
			protected boolean accept(String binding,
										@SuppressWarnings("hiding") Document document,
										Document parentDocument,
										Relation parentRelation,
										Bean bean) 
			throws DomainException, MetaDataException {
				String attributeBinding = null;

				try {
					// set the transient attributes in the beanToSave
					for (Attribute attribute : document.getAllAttributes()) {
						String attributeName = attribute.getName();
						if ((! attribute.isPersistent()) && (! Bean.BIZ_KEY.equals(attributeName))) {
							attributeBinding = attributeName;
							if (binding.length() > 0) {
								attributeBinding = new StringBuilder(64).append(binding).append('.').append(attributeName).toString();
							}
							if (attribute instanceof Collection) {
								@SuppressWarnings("unchecked")
								List<Bean> mergedCollection = (List<Bean>) BindUtil.get(savedBean, attributeBinding);
								@SuppressWarnings("unchecked")
								List<Bean> transientCollection = (List<Bean>) BindUtil.get(unmergedBean, attributeBinding);

								// ensure that we do not try to add the elements
								// of a collection to itself
								if (mergedCollection != transientCollection) {
									mergedCollection.clear();
									mergedCollection.addAll(transientCollection);
								}
							}
							else {
								BindUtil.set(savedBean, attributeBinding, BindUtil.get(unmergedBean, attributeBinding));
							}
						}
					}
				}
				catch (Exception e) {
					if (e instanceof DomainException) {
						throw (DomainException) e;
					}
					else if (e instanceof MetaDataException) {
						throw (MetaDataException) e;
					}
					else {
						throw new DomainException("replaceTransientProperties() cannot set transient property " + attributeBinding, e);
					}
				}
				return true;
			}

		}.visit(document, unmergedBean, customer);
	}

	/**
	 * Check the unique constraints for a document bean.
	 * 
	 * @param document
	 * @param bean
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	private void checkUniqueConstraints(Document document, Bean bean) 
	throws DomainException, MetaDataException {
		String owningModuleName = document.getOwningModuleName();
		String documentName = document.getName();
		String entityName = getDocumentEntityName(owningModuleName, documentName);
		Customer customer = user.getCustomer();

		try {
			for (UniqueConstraint constraint : document.getAllUniqueConstraints()) {
				StringBuilder queryString = new StringBuilder(48);
				queryString.append("select bean from ").append(entityName).append(" as bean");
				
				setFilters(document, constraint.getScope().toDocumentPermissionScope());

				// indicates if we have appended any where clause conditions
				boolean noWhere = true;
	
				// Don't check unique constraints if one of the parameters is a transient bean.
				// The query will produce an error and there is no use anyway as there cannot possibly
				// be unique constraint violation.
				boolean abortCheck = false;
				List<Object> constraintFieldValues = new ArrayList<>();
				for (String fieldName : constraint.getFieldNames()) {
					Object constraintFieldValue = null;
					try {
						constraintFieldValue = BindUtil.get(bean, fieldName);
					}
					catch (Exception e) {
						throw new DomainException(e);
					}
					// Don't do the test if the query parameters are not persisted
					if ((constraintFieldValue instanceof PersistentBean) && (! isPersisted((Bean) constraintFieldValue))) {
						abortCheck = true;
						break; // stop checking the field names of this constraint
					}
					constraintFieldValues.add(constraintFieldValue);
					if (noWhere) {
						queryString.append(" where bean.");
						noWhere = false;
					}
					else {
						queryString.append(" and bean.");
					}
					queryString.append(fieldName);
					queryString.append(" = ?");
				}
	
				if (abortCheck) {
					continue; // iterate to next constraint
				}

				Query query = session.createQuery(queryString.toString());
				if (UtilImpl.QUERY_TRACE) {
					StringBuilder log = new StringBuilder(256);
					log.append("TEST CONSTRAINT ").append(owningModuleName).append('.').append(documentName).append('.').append(constraint.getName());
					log.append(" using ").append(queryString);
					Util.LOGGER.info(log.toString());
				}
				query.setLockMode("bean", LockMode.READ); // take a read lock on all referenced documents
				int index = 0;
				for (@SuppressWarnings("unused") String fieldName : constraint.getFieldNames()) {
					query.setParameter(index, constraintFieldValues.get(index));
					if (UtilImpl.QUERY_TRACE) {
						Util.LOGGER.info("    SET PARAM " + index + " = " + constraintFieldValues.get(index));
					}
					index++;
				}
	
				// Use a scrollable result set in case the result set is massive
				ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY);
				try {
					if (results.next()) {
						boolean persistent = isPersisted(bean);
						Bean first = (Bean) results.get()[0];
						if ((! persistent) || // we are inserting and 1 already exists
								results.next() || // more than 1 exists
								(persistent && (! first.getBizId().equals(bean.getBizId())))) { // updating, and 1 exists that is not this ID
							String message = null;
							try {
								message = BindUtil.formatMessage(customer, constraint.getMessage(), bean);
							}
							catch (Exception e) {
								e.printStackTrace();
								message = "Unique Constraint Violation occurred but could not display the unique constraint message for constraint " +
												constraint.getName();
							}
	
							throw new UniqueConstraintViolationException(constraint.getName(), message);
						}
					}
				}
				finally {
					results.close();
				}
			}
		}
		finally {
			resetFilters(document);
		}
	}

	/**
	 * entity name -> set of beans that are being deleted during the delete operation.
	 * This attribute holds all beans being deleted during the delete operation, by entity name,
	 * so that when we check referential integrity, we can ensure that we do not include links to these beans
	 * which will be deleted (by cascading) anyway.
	 * 
	 * This variable is cleared at the end of the delete operation.
	 * See the finally block below.
	 * The beans to delete are collected by delete() firstly.
	 * The referential integrity test is done in the preDelete() callback.
	 */
	private Map<String, Set<Bean>> beansToDelete = new TreeMap<>();

	@Override
	@SuppressWarnings("unchecked")
	public final <T extends PersistentBean> void delete(Document document, T bean)
	throws DomainException, MetaDataException {
		T newBean = bean;
		
		if (isPersisted(newBean)) {
			try {
				CustomerImpl internalCustomer = (CustomerImpl) getUser().getCustomer();
				boolean vetoed = internalCustomer.interceptBeforeDelete(document, newBean);
				if (! vetoed) {
					// need to merge before validation to ensure that the FK constraints
					// can check for members of collections etc - need the persistent version for this
					String entityName = getDocumentEntityName(document.getOwningModuleName(), document.getName());
					newBean = (T) session.merge(entityName, newBean);
					em.flush();
	
					session.delete(entityName, newBean);
					em.flush();
				
					internalCustomer.interceptAfterDelete(document, newBean);
				}
			}
			catch (Throwable t) {
				treatPersistenceThrowable(t, OperationType.update, newBean.getBizLock());
			}
			finally {
				beansToDelete.clear();
			}
		}
	}

	// Do not increase visibility of this method as we don't want it to be public.
	private void checkReferentialIntegrityOnDelete(Document document, 
													PersistentBean beanToDelete, 
													Set<String> documentsVisited,
													Map<String, Set<Bean>> beansToBeCascaded)
	throws DomainException, MetaDataException {
		Customer customer = user.getCustomer();
		List<ExportedReference> refs = ((CustomerImpl) customer).getExportedReferences(document);
		if (refs != null) {
			for (ExportedReference ref : refs) {
				ReferenceType type = ref.getType();
				// Need to check aggregation FKs
				// Need to check collection joining table element_id FKs
				// but do NOT need to check child collection parent_ids as they point back
				if (! CollectionType.child.equals(type)) {
					String moduleName = ref.getModuleName();
					String documentName = ref.getDocumentName();
					String entityName = getDocumentEntityName(moduleName, documentName);
					Module referenceModule = customer.getModule(moduleName);
					Document referenceDocument = referenceModule.getDocument(customer, documentName);
					Persistent persistent = document.getPersistent();
					if (persistent != null) {
						if (ExtensionStrategy.mapped.equals(persistent.getStrategy())) {
							checkMappedReference(beanToDelete, beansToBeCascaded, document, ref, entityName, referenceDocument);
						}
						else {
							checkTypedReference(beanToDelete, beansToBeCascaded, document, ref, entityName, referenceDocument);
						}
					}
				}
			}
		}

		documentsVisited.add(new StringBuilder(32).append(document.getOwningModuleName()).append('.').append(document.getName()).toString());

		// Process base document if present
		String baseDocumentName = ((CustomerImpl) customer).getBaseDocument(document);
		if ((baseDocumentName != null) && (! documentsVisited.contains(baseDocumentName))) {
			int dotIndex = baseDocumentName.indexOf('.');
			Module baseModule = customer.getModule(baseDocumentName.substring(0, dotIndex));
			Document baseDocument = baseModule.getDocument(customer, baseDocumentName.substring(dotIndex + 1));
			checkReferentialIntegrityOnDelete(baseDocument, beanToDelete, documentsVisited, beansToBeCascaded);
		}

		// Process derived documents if present
		for (String derivedDocumentName : ((CustomerImpl) customer).getDerivedDocuments(document)) {
			if ((derivedDocumentName != null) && (! documentsVisited.contains(derivedDocumentName))) {
				int dotIndex = derivedDocumentName.indexOf('.');
				Module derivedModule = customer.getModule(derivedDocumentName.substring(0, dotIndex));
				Document derivedDocument = derivedModule.getDocument(customer, derivedDocumentName.substring(dotIndex + 1));
				checkReferentialIntegrityOnDelete(derivedDocument, beanToDelete, documentsVisited, beansToBeCascaded);
			}
		}
	}

	private void checkTypedReference(PersistentBean beanToDelete, 
										Map<String, Set<Bean>> beansToBeCascaded,
										Document document,
										ExportedReference ref,
										String entityName,
										Document referenceDocument)
	throws DomainException, MetaDataException {
		if (ExtensionStrategy.mapped.equals(referenceDocument.getPersistent().getStrategy())) {
			// Find all implementations below the mapped and check these instead
			Set<Document> derivations = new HashSet<>();
			populateImmediateMapImplementingDerivations((CustomerImpl) user.getCustomer(), referenceDocument, derivations);
			for (Document derivation : derivations) {
				checkTypedReference(beanToDelete, beansToBeCascaded, document, ref, entityName, derivation);
			}
		}
		else {
			setFilters(referenceDocument, DocumentPermissionScope.global);
			try {
				StringBuilder queryString = new StringBuilder(64);
				queryString.append("select bean from ");
				// NB Don't ever change this to the entityName parameter as this method can be called recusrively above.
				//    The entityName is used to find cascaded beans to exclude from the integrity test
				queryString.append(getDocumentEntityName(referenceDocument.getOwningModuleName(), referenceDocument.getName()));
				queryString.append(" as bean");
				if (ref.isCollection()) {
					queryString.append(" where :referencedBean member of bean.");
					queryString.append(ref.getReferenceFieldName());
				}
				else {
					queryString.append(" where bean.");
					queryString.append(ref.getReferenceFieldName());
					queryString.append(" = :referencedBean");
				}
				
				Set<Bean> theseBeansToBeCascaded = beansToBeCascaded.get(entityName);
				if (theseBeansToBeCascaded != null) {
					int i = 0;
					for (@SuppressWarnings("unused") Bean thisBeanToBeCascaded : theseBeansToBeCascaded) {
						queryString.append(" and bean != :deletedBean").append(i++);
					}
				}
				if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("FK check : " + queryString);
	
				Query query = session.createQuery(queryString.toString());
				query.setLockMode("bean", LockMode.READ); // read lock required for referential integrity
				query.setEntity("referencedBean", beanToDelete);
				if (theseBeansToBeCascaded != null) {
					int i = 0;
					for (Bean thisBeanToBeCascaded : theseBeansToBeCascaded) {
						query.setEntity("deletedBean" + i++, thisBeanToBeCascaded);
					}
				}
	
				ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY);
				try {
					if (results.next()) {
						throw new ReferentialConstraintViolationException("Cannot delete " + document.getSingularAlias() + 
																			" \"" + beanToDelete.getBizKey() + 
																			"\" as it is referenced by a " + ref.getDocumentAlias());
					}
				}
				finally {
					results.close();
				}
			}
			finally {
				resetFilters(referenceDocument);
			}
		}
	}
	
	private void checkMappedReference(PersistentBean beanToDelete, 
										Map<String, Set<Bean>> beansToBeCascaded,
										Document document,
										ExportedReference ref,
										String entityName,
										Document referenceDocument)
	throws DomainException, MetaDataException {
		if (ExtensionStrategy.mapped.equals(referenceDocument.getPersistent().getStrategy())) {
			// Find all implementations below the mapped and check these instead
			Set<Document> derivations = new HashSet<>();
			populateImmediateMapImplementingDerivations((CustomerImpl) user.getCustomer(), referenceDocument, derivations);
			for (Document derivation : derivations) {
				checkMappedReference(beanToDelete, beansToBeCascaded, document, ref, entityName, derivation);
			}
		}
		else {
			StringBuilder queryString = new StringBuilder(64);
			queryString.append("select 1 from ");
			queryString.append(referenceDocument.getPersistent().getPersistentIdentifier());
			if (ref.isCollection()) {
				queryString.append('_').append(ref.getReferenceFieldName());
				queryString.append(" where element_id = :reference_id");
			}
			else {
				queryString.append(" where ").append(ref.getReferenceFieldName());
				queryString.append("_id = :reference_id");
			}
			
			Set<Bean> theseBeansToBeCascaded = beansToBeCascaded.get(entityName);
			if (theseBeansToBeCascaded != null) {
				int i = 0;
				for (@SuppressWarnings("unused") Bean thisBeanToBeCascaded : theseBeansToBeCascaded) {
					if (ref.isCollection()) {
						queryString.append(" and owner_id != :deleted_id");
					}
					else {
						queryString.append(" and bizId != :deleted_id");
					}
					queryString.append(i++);
				}
			}
			if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("FK check : " + queryString);
	
			SQLQuery query = session.createSQLQuery(queryString.toString());
//			query.setLockMode("bean", LockMode.READ); // read lock required for referential integrity
			query.setString("reference_id", beanToDelete.getBizId());
			if (theseBeansToBeCascaded != null) {
				int i = 0;
				for (Bean thisBeanToBeCascaded : theseBeansToBeCascaded) {
					query.setEntity("deleted_id" + i++, thisBeanToBeCascaded.getBizId());
				}
			}
	
			ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY);
			try {
				if (results.next()) {
					throw new ReferentialConstraintViolationException("Cannot delete " + document.getSingularAlias() + 
																		" \"" + beanToDelete.getBizKey() + 
																		"\" as it is referenced by a " + ref.getDocumentAlias());
				}
			}
			finally {
				results.close();
			}
		}
	}

	private void populateImmediateMapImplementingDerivations(CustomerImpl customer,
																Document document,
																Set<Document> result)
	throws MetaDataException {
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
	
	@Override
	@SuppressWarnings("unchecked")
	public final <T extends Bean> T retrieve(Document document, String id, boolean forUpdate)
	throws DomainException, MetaDataException {
		T result = null;
		Class<?> beanClass = null;
		String entityName = getDocumentEntityName(document.getOwningModuleName(), document.getName());
		Customer customer = user.getCustomer();
		try {
			if (UtilImpl.USING_JPA && (! entityName.startsWith(customer.getName()))) {
				beanClass = ((DocumentImpl) document).getBeanClass(customer);
			}

			if (forUpdate) {
				if (beanClass != null) {
					result = (T) session.load(beanClass, id, LockMode.UPGRADE);
				}
				else {
					result = (T) session.load(entityName, id, LockMode.UPGRADE);
				}
			}
			else // works with transient instances
			{
				if (beanClass != null) {
					result = (T) em.find(beanClass, id);
				}
				else {
					result = (T) session.get(entityName, id);
				}
			}
		}
		catch (StaleObjectStateException e) // thrown from session.load() with LockMode.UPGRADE
		{
			// Database was updated by another user.
			// The select for update is by [bizId] and [bizVersion] and other transaction changed the bizVersion
			// so it cannot be found.
			// The result is null here, so retrieve it again (from the database) without trying to lock
			if (beanClass != null) {
				result = (T) em.find(beanClass, id);
			}
			else {
				result = (T) session.get(entityName, id);
			}

			session.refresh(result, LockMode.UPGRADE);
		}
		catch (ClassNotFoundException e) {
			throw new MetaDataException("Could not find bean", e);
		}

		return result;
	}

	@Override
	public void postLoad(AbstractPersistentBean loadedBean)
	throws Exception {
		Customer customer = user.getCustomer();
		CustomerImpl internalCustomer = (CustomerImpl) customer;
		boolean vetoed = internalCustomer.interceptBeforePostLoad(loadedBean);
		if (! vetoed) {
			Document document = customer.getModule(loadedBean.getBizModule()).getDocument(customer, loadedBean.getBizDocument());
			Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
			if (bizlet != null) {
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "postLoad", "Entering " + bizlet.getClass().getName() + ".postLoad: " + loadedBean);
				bizlet.postLoad(loadedBean);
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "postLoad", "Exiting " + bizlet.getClass().getName() + ".postLoad");
			}
			internalCustomer.interceptAfterPostLoad(loadedBean);
		}

		// clear the object's dirtiness
		loadedBean.originalValues().clear();
	}

	@Override
	public void reindex(PersistentBean beanToReindex)
	throws Exception {
		BeanContent content = new BeanContent(beanToReindex);
		Map<String, String> properties = content.getProperties();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(beanToReindex.getBizModule());
		Document document = module.getDocument(customer, beanToReindex.getBizDocument());
		for (Attribute attribute : document.getAllAttributes()) {
			if (attribute instanceof Field) {
				Field field = (Field) attribute;
				AttributeType type = attribute.getAttributeType();
				IndexType index = field.getIndex();
				if (IndexType.textual.equals(index) || IndexType.both.equals(index)) {
					String fieldName = field.getName();
					String value = BindUtil.getDisplay(customer, beanToReindex, fieldName);
					if (AttributeType.markup.equals(type)) {
						value = extractTextFromMarkup(value);
					}
					value = Util.processStringValue(value);
					if (value != null) {
						properties.put(fieldName, value);
					}
				}
			}
		}

		if (properties.isEmpty()) {
			removeBeanContent(beanToReindex);
		}
		else {
			putBeanContent(content);
		}
	}

	public void index(AbstractPersistentBean beanToIndex,
							String[] propertyNames,
							Type[] propertyTypes,
							Object[] oldState,
							Object[] state)
	throws Exception {
		BeanContent content = new BeanContent(beanToIndex);
		Map<String, String> properties = content.getProperties();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(beanToIndex.getBizModule());
		Document document = module.getDocument(customer, beanToIndex.getBizDocument());

		String oldBizDataGroupId = beanToIndex.getBizDataGroupId();
		String oldBizUserId = beanToIndex.getBizUserId();
		for (int i = 0, l = propertyNames.length; i < l; i++) {
			String propertyName = propertyNames[i];
			if (oldState != null) { // an update
				if (Bean.DATA_GROUP_ID.equals(propertyName)) {
					oldBizDataGroupId = (String) oldState[i];
				}
				else if (Bean.USER_ID.equals(propertyName)) {
					oldBizUserId = (String) oldState[i];
				}
			}
			// NB use getMetaForBinding() to ensure that base document attributes are also retrieved
			TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, propertyName);
			Attribute attribute = target.getAttribute();
			if (attribute instanceof Field) {
				Field field = (Field) attribute;
				AttributeType type = field.getAttributeType();
				IndexType index = field.getIndex();
				if (IndexType.textual.equals(index) || IndexType.both.equals(index)) {
					if (oldState != null) { // an update
						if (! propertyTypes[i].isEqual(state[i], oldState[i], EntityMode.POJO)) {
							String value = (state[i] == null) ? null : state[i].toString();
							if (value != null) {
								if (AttributeType.markup.equals(type)) {
									value = extractTextFromMarkup(value);
								}
								
								properties.put(propertyName, value);
							}
						}
					}
					else {
						String value = (state[i] == null) ? null : state[i].toString();
						if (value != null) {
							if (AttributeType.markup.equals(type)) {
								value = extractTextFromMarkup(value);
							}
							
							properties.put(propertyName, value);
						}
					}
				}
				if (AttributeType.content.equals(type)) {
					if (oldState != null) { // an update
						if ((state[i] == null) && (oldState[i] != null)) { // removed the content link
							// Remove the attachment content
							removeAttachmentContent((String) oldState[i]);
						}
					}
				}
			}
		}

		// Move the node if the user id or data group id has changed
		if (oldState != null) { // an update
			String bizDataGroupId = beanToIndex.getBizDataGroupId();
			String bizUserId = beanToIndex.getBizUserId();

			if (((bizDataGroupId == null) && (oldBizDataGroupId != null)) || // null to not null
					((bizDataGroupId != null) && (! bizDataGroupId.equals(oldBizDataGroupId))) || // not the same
					((bizUserId == null) && (oldBizUserId != null)) || // null to not null
					((bizUserId != null) && (! bizUserId.equals(oldBizUserId)))) { // not the same
				moveBeanContent(content, oldBizDataGroupId, oldBizUserId);
			}
		}

		if (! properties.isEmpty()) {
			putBeanContent(content);
		}
	}

	private static final Tika TIKA = new Tika();
	
	private static String extractTextFromMarkup(String value) throws IOException, TikaException {
		return TIKA.parseToString(new ByteArrayInputStream(value.getBytes()));
	}
	
	// Need the callback because an element deleted from a collection will be deleted and only this event will pick it up
	@Override
	@SuppressWarnings("synthetic-access")
	public void preRemove(AbstractPersistentBean beanToDelete)
	throws Exception {
		final Customer customer = user.getCustomer();
		Module module = customer.getModule(beanToDelete.getBizModule());
		Document document = module.getDocument(customer, beanToDelete.getBizDocument());
		
		// Collect beans to be cascaded
		new CascadeDeleteBeanVisitor() {
			@Override
			public void preDeleteProcessing(Document documentToCascade, Bean beanToCascade) 
			throws Exception {
				add(documentToCascade, beanToCascade);
			}
			
			private void add(Document documentToCascade, Bean beanToCascade) 
			throws Exception {
				String entityName = AbstractHibernatePersistence.this.getDocumentEntityName(documentToCascade.getOwningModuleName(),
																								documentToCascade.getName());
				Set<Bean> theseBeansToDelete = beansToDelete.get(entityName);
				if (theseBeansToDelete == null) {
					theseBeansToDelete = new TreeSet<>();
					beansToDelete.put(entityName, theseBeansToDelete);
				}
				theseBeansToDelete.add(beanToCascade);

				// Ensure that this bean is registered against any entity names defined in its base documents too
				Extends inherits = documentToCascade.getExtends();
				if (inherits != null) {
					Document baseDocument = customer.getModule(documentToCascade.getOwningModuleName()).getDocument(customer, inherits.getDocumentName());
					add(baseDocument, beanToCascade);
				}
			}
		}.visit(document, beanToDelete, customer);
		
		try {
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePreDelete(beanToDelete);
			if (! vetoed) {
				Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preDelete", "Entering " + bizlet.getClass().getName() + ".preDelete: " + beanToDelete);
					bizlet.preDelete(beanToDelete);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preDelete", "Exiting " + bizlet.getClass().getName() + ".preDelete");
				}
				internalCustomer.interceptAfterPreDelete(beanToDelete);
			}

			Set<String> documentsVisited = new TreeSet<>();
			checkReferentialIntegrityOnDelete(document,
												beanToDelete,
												documentsVisited,
												beansToDelete);
			((PersistentBean) beanToDelete).setBizLock(new OptimisticLock(user.getName(), new Date()));
		}
		catch (ValidationException e) {
			for (Message message : e.getMessages()) {
				ValidationUtil.processErrorMessageBindings(customer, message, beanToDelete, beanToDelete);
			}
			throw e;
		}
	}

	@Override
	public void postRemove(AbstractPersistentBean loadedBean)
	throws Exception {
		removeBeanContent(loadedBean);
	}
	
	@SuppressWarnings("deprecation")
	public final Connection getConnection() {
		return session.connection();
	}
	
	private static final Integer NEW_VERSION = new Integer(0);
	private static final String CHILD_PARENT_ID = ChildBean.PARENT_NAME + "_id";
	
	@Override
	public void upsertBeanTuple(PersistentBean bean)
	throws DomainException, MetaDataException {
		CustomerImpl customer = (CustomerImpl) user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		String parentDocumentName = document.getParentDocumentName();
		String bizDiscriminator = null;
		StringBuilder query = new StringBuilder(256);

		// Get all attributes that are required for the table backing this document
		// including any single or mapped inheritance
		List<Attribute> attributes = new ArrayList<>(document.getAttributes());
		Extends inherits = document.getExtends();
		while (inherits != null) {
			Module baseModule = customer.getModule(document.getOwningModuleName());
			Document baseDocument = baseModule.getDocument(customer, inherits.getDocumentName());
			Persistent basePersistent = baseDocument.getPersistent();
			if (basePersistent != null) {
				ExtensionStrategy baseStrategy = basePersistent.getStrategy();
				if (ExtensionStrategy.single.equals(baseStrategy) || ExtensionStrategy.mapped.equals(baseStrategy)) {
					attributes.addAll(baseDocument.getAttributes());
				}
			}
			inherits = baseDocument.getExtends();
		}

		// now get on with the upsert
		
		if (bean.isPersisted()) { // update an existing row
			query.append("update ").append(document.getPersistent().getPersistentIdentifier()).append(" set ");
			query.append(PersistentBean.VERSION_NAME).append('=').append(PersistentBean.VERSION_NAME).append("+1");
			query.append(',').append(PersistentBean.LOCK_NAME).append("=:").append(PersistentBean.LOCK_NAME);
			query.append(',').append(Bean.DATA_GROUP_ID).append("=:").append(Bean.DATA_GROUP_ID);
			query.append(',').append(Bean.BIZ_KEY).append("=:").append(Bean.BIZ_KEY);
			if (parentDocumentName != null) {
				if (parentDocumentName.equals(document.getName())) {
					query.append(',').append(HierarchicalBean.PARENT_ID).append("=:").append(HierarchicalBean.PARENT_ID);
				}
				else {
					query.append(',').append(CHILD_PARENT_ID).append("=:").append(CHILD_PARENT_ID);
				}
			}

			for (Attribute attribute : attributes) {
				if (! attribute.isPersistent()) {
					continue;
				}

				String attributeName = attribute.getName();
				if (Bean.BIZ_KEY.equals(attributeName)) {
					continue;
				}
				else if (attribute instanceof Association) {
					query.append(',').append(attributeName).append("_id=:").append(attributeName).append("_id");
				}
				else if (attribute instanceof Field) {
					query.append(',').append(attributeName).append("=:").append(attributeName);
				}
			}

			query.append(" where ").append(Bean.DOCUMENT_ID).append("=:").append(Bean.DOCUMENT_ID);
		}
		else { // insert a new row
			// Add the built ins
			StringBuilder columns = new StringBuilder(128);
			columns.append(Bean.DOCUMENT_ID).append(',').append(PersistentBean.VERSION_NAME).append(',');
			columns.append(PersistentBean.LOCK_NAME).append(',').append(Bean.CUSTOMER_NAME).append(',');
			columns.append(Bean.DATA_GROUP_ID).append(',').append(Bean.BIZ_KEY).append(',').append(Bean.USER_ID);
			StringBuilder values = new StringBuilder(128);
			values.append(':').append(Bean.DOCUMENT_ID).append(",:").append(PersistentBean.VERSION_NAME).append(",:");
			values.append(PersistentBean.LOCK_NAME).append(",:").append(Bean.CUSTOMER_NAME).append(",:");
			values.append(Bean.DATA_GROUP_ID).append(",:").append(Bean.BIZ_KEY).append(",:").append(Bean.USER_ID);

			// Add parent if required
			if (parentDocumentName != null) {
				if (parentDocumentName.equals(document.getName())) {
					columns.append(',').append(HierarchicalBean.PARENT_ID);
					values.append(",:").append(HierarchicalBean.PARENT_ID);
				}
				else {
					columns.append(',').append(CHILD_PARENT_ID);
					values.append(",:").append(CHILD_PARENT_ID);
				}
			}
			
			// Add bizDiscriminator if required
			Persistent persistent = document.getPersistent();
			if (persistent != null) {
				if (ExtensionStrategy.single.equals(persistent.getStrategy())) {
					bizDiscriminator = persistent.getDiscriminator();
					if (bizDiscriminator == null) {
						bizDiscriminator = new StringBuilder(64).append(module.getName()).append(document.getName()).toString();
					}
					columns.append(',').append(PersistentBean.DISCRIMINATOR_NAME);
					values.append(",:").append(PersistentBean.DISCRIMINATOR_NAME);
				}
			}
					
			// Add fields and associations
			for (Attribute attribute : attributes) {
				if (! attribute.isPersistent()) {
					continue;
				}

				String attributeName = attribute.getName();
				if (Bean.BIZ_KEY.equals(attributeName)) {
					continue;
				}
				else if (attribute instanceof Association) {
					columns.append(',').append(attributeName).append("_id");
					values.append(",:").append(attributeName).append("_id");
				}
				else if (attribute instanceof Field) {
					columns.append(',').append(attributeName);
					values.append(",:").append(attributeName);
				}
			}

			// build the query
			query.append(" insert into ").append(document.getPersistent().getPersistentIdentifier()).append(" (");
			query.append(columns).append(") values (").append(values).append(')');
		}

		// bind the built in parameters
		SQL sql = newSQL(query.toString());
		sql.putParameter(Bean.DOCUMENT_ID, bean.getBizId(), false);
		bean.setBizLock(new OptimisticLock(user.getName(), new Date()));
		sql.putParameter(PersistentBean.LOCK_NAME, bean.getBizLock().toString(), false);
		if (! bean.isPersisted()) {
			sql.putParameter(PersistentBean.VERSION_NAME, NEW_VERSION);
			sql.putParameter(Bean.CUSTOMER_NAME, bean.getBizCustomer(), false);
			sql.putParameter(Bean.USER_ID, bean.getBizUserId(), false);
		}
		sql.putParameter(Bean.DATA_GROUP_ID, bean.getBizDataGroupId(), false);
		sql.putParameter(Bean.BIZ_KEY, Util.processStringValue(bean.getBizKey()), false);

		// Bind parent if required
		if (parentDocumentName != null) {
			if (parentDocumentName.equals(document.getName())) {
				sql.putParameter(HierarchicalBean.PARENT_ID, ((HierarchicalBean<?>) bean).getBizParentId(), false);
			}
			else {
				sql.putParameter(CHILD_PARENT_ID, ((ChildBean<?>) bean).getParent().getBizId(), false);
			}
		}

		// Bind discriminator if required
		if (bizDiscriminator != null) {
			sql.putParameter(PersistentBean.DISCRIMINATOR_NAME, bizDiscriminator, false);
		}
		
		// Bind fields and associations
		for (Attribute attribute : attributes) {
			if (! attribute.isPersistent()) {
				continue;
			}
			String attributeName = attribute.getName();
			if (Bean.BIZ_KEY.equals(attributeName)) {
				continue;
			}

			try {
				if (attribute instanceof Association) {
					String columnName = new StringBuilder(64).append(attributeName).append("_id").toString();
					String binding = new StringBuilder(64).append(attributeName).append('.').append(Bean.DOCUMENT_ID).toString();
					sql.putParameter(columnName, (String) BindUtil.get(bean, binding), false);
				}
				else if (attribute instanceof Enumeration) {
					org.skyve.domain.types.Enumeration value = (org.skyve.domain.types.Enumeration) BindUtil.get(bean, attributeName);
					sql.putParameter(attributeName, value);
				}
				else if (attribute instanceof Field) {
					List<DomainValue> domainValues = null;
					DomainType domainType = attribute.getDomainType();
					if (domainType != null) {
						domainValues = ((DocumentImpl) document).getDomainValues(customer, domainType, attribute, bean);
					}
					Object value = BindUtil.get(bean, attributeName);
					if (domainValues != null) {
						for (DomainValue domainValue : domainValues) {
							if (domainValue.getDescription().equals(value)) {
								value = domainValue.getCode();
								break;
							}
						}
					}
					sql.putParameter(attributeName, value, attribute.getAttributeType());
				}
			}
			catch (Exception e) {
				throw new DomainException("Could not grab the value in attribute " + attributeName +
											" from bean " + bean, e);
			}
		}

		// execute it
		sql.execute();
		
		// Set the bizVersion appropriately, if the upsert was successful
		Integer bizVersion = bean.getBizVersion();
		bean.setBizVersion((bizVersion == null) ? NEW_VERSION : Integer.valueOf(bizVersion.intValue() + 1));
	}

	@Override
	public void upsertCollectionTuples(PersistentBean owningBean, String collectionName)
	throws DomainException, MetaDataException {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(owningBean.getBizModule());
		Document document = module.getDocument(customer, owningBean.getBizDocument());
		StringBuilder query = new StringBuilder(256);

		List<PersistentBean> elementBeans = null;
		try {
			@SuppressWarnings("unchecked")
			List<PersistentBean> list = (List<PersistentBean>) BindUtil.get(owningBean, collectionName);
			elementBeans = list;
		}
		catch (Exception e) {
			throw new DomainException("Could not get collection " + collectionName + 
										" from bean " + owningBean, e);
		}
		
		for (Bean elementBean : elementBeans) {
			query.append("select * from ").append(document.getPersistent().getPersistentIdentifier()).append('_').append(collectionName);
			query.append(" where owner_id=:owner_id and element_id=:element_id");

			SQL sql = newSQL(query.toString());
			sql.putParameter("owner_id", owningBean.getBizId(), false);
			sql.putParameter("element_id", elementBean.getBizId(), false);

			boolean notExists = sql.tupleResults().isEmpty();
			query.setLength(0);
			if (notExists) {
				query.append("insert into ").append(document.getPersistent().getPersistentIdentifier()).append('_').append(collectionName);
				query.append(" (owner_id,element_id) values (:owner_id,:element_id)");

				sql = newSQL(query.toString());
				sql.putParameter("owner_id", owningBean.getBizId(), false);
				sql.putParameter("element_id", elementBean.getBizId(), false);

				sql.execute();
				query.setLength(0);
			}
		}
	}
	
	@Override
	public void insertCollectionTuples(PersistentBean owningBean, String collectionName)
	throws DomainException, MetaDataException {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(owningBean.getBizModule());
		Document document = module.getDocument(customer, owningBean.getBizDocument());
		StringBuilder query = new StringBuilder(256);
		query.append("insert into ").append(document.getPersistent().getPersistentIdentifier()).append('_').append(collectionName);
		query.append(" (owner_id,element_id) values (:owner_id,:element_id)");

		List<PersistentBean> elementBeans = null;
		try {
			@SuppressWarnings("unchecked")
			List<PersistentBean> list = (List<PersistentBean>) BindUtil.get(owningBean, collectionName);
			elementBeans = list;
		}
		catch (Exception e) {
			throw new DomainException("Could not get collection " + collectionName + 
										" from bean " + owningBean, e);
		}
		
		for (Bean elementBean : elementBeans) {
			SQL sql = newSQL(query.toString());
			sql.putParameter("owner_id", owningBean.getBizId(), false);
			sql.putParameter("element_id", elementBean.getBizId(), false);

			sql.execute();
		}
	}

	/**
	 * In case of emergency, break glass
	 */
	public final EntityManager getEntityManager() {
		return em;
	}
	
	/**
	 * In case of emergency, break glass
	 */
	public final Session getSession() {
		return session;
	}

	@Override
	public SQL newSQL(String query) {
		return new HibernateSQL(query, this);
	}

	@Override
	public SQL newNamedSQL(String moduleName, String queryName)
	throws MetaDataException {
		Module module = user.getCustomer().getModule(moduleName);
		return new HibernateSQL(module.getSQL(queryName).getQuery(), this);
	}

	@Override
	public SQL newNamedSQL(Module module, String queryName) {
		return new HibernateSQL(module.getSQL(queryName).getQuery(), this);
	}

	@Override
	public SQL newSQL(String moduleName, String documentName, String query) {
		return new HibernateSQL(moduleName, documentName, query, this);
	}

	@Override
	public SQL newSQL(Document document, String query) {
		return new HibernateSQL(document, query, this);
	}

	@Override
	public SQL newNamedSQL(String moduleName, String documentName, String queryName)
	throws MetaDataException {
		Module module = user.getCustomer().getModule(moduleName);
		return new HibernateSQL(moduleName, documentName, module.getSQL(queryName).getQuery(), this);
	}

	@Override
	public SQL newNamedSQL(Document document, String queryName) 
	throws MetaDataException {
		Module module = user.getCustomer().getModule(document.getOwningModuleName());
		return new HibernateSQL(document, module.getSQL(queryName).getQuery(), this);
	}

	@Override
	public BizQL newBizQL(String query) {
		return new HibernateBizQL(query, this);
	}

	@Override
	public BizQL newNamedBizQL(String moduleName, String queryName) 
	throws MetaDataException {
		Module module = user.getCustomer().getModule(moduleName);
		return new HibernateBizQL(module.getBizQL(queryName).getQuery(), this);
	}

	@Override
	public BizQL newNamedBizQL(Module module, String queryName) {
		return new HibernateBizQL(module.getBizQL(queryName).getQuery(), this);
	}

	@Override
	public DocumentQuery newNamedDocumentQuery(String moduleName, String queryName)
	throws MetaDataException {
		Module module = user.getCustomer().getModule(moduleName);
		DocumentQueryDefinition query = module.getDocumentQuery(queryName);
		return query.constructDocumentQuery(null, null);
	}

	@Override
	public DocumentQuery newNamedDocumentQuery(Module module, String queryName)
	throws MetaDataException {
		DocumentQueryDefinition query = module.getDocumentQuery(queryName);
		return query.constructDocumentQuery(null, null);
	}

	@Override
	public DocumentQuery newDocumentQuery(Document document) {
		return new HibernateDocumentQuery(document, this);
	}

	@Override
	public DocumentQuery newDocumentQuery(String moduleName, String documentName)
	throws MetaDataException {
		return new HibernateDocumentQuery(moduleName, documentName, this);
	}

	@Override
	public DocumentQuery newDocumentQuery(Document document, String fromClause, String filterClause) {
		return new HibernateDocumentQuery(document, fromClause, filterClause, this);
	}

	@Override
	public DocumentQuery newDocumentQuery(Bean queryByExampleBean)
	throws Exception {
		return new HibernateDocumentQuery(queryByExampleBean, this);
	}
}
