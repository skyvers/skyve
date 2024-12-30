package org.skyve.impl.persistence.hibernate;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.function.Function;

import javax.cache.management.CacheStatisticsMXBean;

import org.apache.commons.codec.digest.DigestUtils;
import org.hibernate.Filter;
import org.hibernate.FlushMode;
import org.hibernate.HibernateException;
import org.hibernate.LockMode;
import org.hibernate.MappingException;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.StaleObjectStateException;
import org.hibernate.boot.Metadata;
import org.hibernate.boot.MetadataSources;
import org.hibernate.boot.SessionFactoryBuilder;
import org.hibernate.boot.cfgxml.spi.LoadedConfig;
import org.hibernate.boot.registry.StandardServiceRegistry;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.boot.spi.MetadataImplementor;
import org.hibernate.cache.CacheException;
import org.hibernate.cfg.AvailableSettings;
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.event.service.spi.EventListenerRegistry;
import org.hibernate.event.spi.EventType;
import org.hibernate.hql.internal.ast.QuerySyntaxException;
import org.hibernate.integrator.spi.Integrator;
import org.hibernate.integrator.spi.IntegratorService;
import org.hibernate.internal.SessionImpl;
import org.hibernate.mapping.Column;
import org.hibernate.query.NativeQuery;
import org.hibernate.query.Query;
import org.hibernate.service.spi.SessionFactoryServiceRegistry;
import org.hibernate.tool.hbm2ddl.SchemaExport;
import org.hibernate.tool.hbm2ddl.SchemaUpdate;
import org.hibernate.tool.schema.TargetType;
import org.hibernate.type.StringType;
import org.hibernate.type.Type;
import org.skyve.EXT;
import org.skyve.content.BeanContent;
import org.skyve.content.TextExtractor;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.messages.OptimisticLockException.OperationType;
import org.skyve.domain.messages.ReferentialConstraintViolationException;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.ExportedReference;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.persistence.hibernate.dialect.DDLDelegate;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.util.CascadeDeleteBeanVisitor;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.ValidationUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DynamicPersistence;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.BeanVisitor;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityNotFoundException;
import jakarta.persistence.EntityTransaction;
import jakarta.persistence.RollbackException;

public abstract class AbstractHibernatePersistence extends AbstractPersistence {
	private static final long serialVersionUID = -1813679859498468849L;

	private static final Logger LOGGER = LoggerFactory.getLogger(AbstractHibernatePersistence.class);
    private static final Logger QUERY_LOGGER = Category.QUERY.logger();
    private static final Logger BIZLET_LOGGER = Category.BIZLET.logger();

	private static SessionFactory sf = null;
	private static Metadata metadata = null;
	private static final Map<String, SkyveDialect> DIALECTS = new TreeMap<>();

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
		em = sf.createEntityManager();
		session = em.unwrap(Session.class);
		session.setHibernateFlushMode(FlushMode.MANUAL);
	}

	protected abstract void removeBeanContent(@Nonnull PersistentBean bean) throws Exception;
	protected abstract void putBeanContent(@Nonnull BeanContent content) throws Exception;
	protected abstract void closeContent() throws Exception;
	
	@Override
	@SuppressWarnings("unchecked")
	public final void disposeAllPersistenceInstances() {
		// remove this instance - and hopefully the only instance running
		commit(true);

		sf.close();
		sf = null;
		metadata = null;

		if (UtilImpl.SKYVE_PERSISTENCE_CLASS == null) {
			AbstractPersistence.IMPLEMENTATION_CLASS = HibernateContentPersistence.class;
		}
		else {
			try {
				AbstractPersistence.IMPLEMENTATION_CLASS = (Class<? extends AbstractPersistence>) Thread.currentThread().getContextClassLoader().loadClass(UtilImpl.SKYVE_PERSISTENCE_CLASS);
			}
			catch (ClassNotFoundException e) {
				throw new IllegalStateException("Could not find SKYVE_PERSISTENCE_CLASS " + UtilImpl.SKYVE_PERSISTENCE_CLASS, e);
			}
		}

		if (UtilImpl.SKYVE_DYNAMIC_PERSISTENCE_CLASS == null) {
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = RDBMSDynamicPersistence.class;
		}
		else {
			try {
				AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = (Class<? extends DynamicPersistence>) Thread.currentThread().getContextClassLoader().loadClass(UtilImpl.SKYVE_DYNAMIC_PERSISTENCE_CLASS);
			}
			catch (ClassNotFoundException e) {
				throw new IllegalStateException("Could not find SKYVE_DYNAMIC_PERSISTENCE_CLASS " + UtilImpl.SKYVE_DYNAMIC_PERSISTENCE_CLASS, e);
			}
		}

		configure();
	}

	@SuppressWarnings("resource")
	private static void configure() {
		LoadedConfig config = LoadedConfig.baseline();
		Map<String, String> cfg = config.getConfigurationValues();
		
		String dataSource = UtilImpl.DATA_STORE.getJndiDataSourceName();
		if (dataSource == null) {
			cfg.put(AvailableSettings.DRIVER, UtilImpl.DATA_STORE.getJdbcDriverClassName());
			cfg.put(AvailableSettings.URL, UtilImpl.DATA_STORE.getJdbcUrl());
			String value = UtilImpl.DATA_STORE.getUserName();
			if (value != null) {
				cfg.put(AvailableSettings.USER, value);
			}
			value = UtilImpl.DATA_STORE.getPassword();
			if (value != null) {
				cfg.put(AvailableSettings.PASS, value);
			}
			cfg.put(AvailableSettings.AUTOCOMMIT, "false");
		}
		else {
			cfg.put(AvailableSettings.DATASOURCE, dataSource);
		}
		cfg.put(AvailableSettings.DIALECT, UtilImpl.DATA_STORE.getDialectClassName());

		// Query Caching screws up pessimistic locking
		cfg.put(AvailableSettings.USE_QUERY_CACHE, "false");

		// turn on second level caching
		cfg.put(AvailableSettings.USE_SECOND_LEVEL_CACHE, "true");
		cfg.put(AvailableSettings.CACHE_REGION_FACTORY, "org.hibernate.cache.jcache.JCacheRegionFactory");
		cfg.put("hibernate.javax.cache.provider", "org.ehcache.jsr107.EhcacheCachingProvider");
		cfg.put("hibernate.javax.cache.missing_cache_strategy", UtilImpl.HIBERNATE_FAIL_ON_MISSING_CACHE ? "fail" : "create");
		
		// Allow more than 1 representation of the same detached entity to be merged,
		// possibly from multiple sessions, multiple caches, or various serializations.
		cfg.put(AvailableSettings.MERGE_ENTITY_COPY_OBSERVER, "allow");
		
		// JDBC parameters
		cfg.put(AvailableSettings.USE_STREAMS_FOR_BINARY, "true");
		cfg.put(AvailableSettings.STATEMENT_BATCH_SIZE, "16");
		cfg.put(AvailableSettings.MAX_FETCH_DEPTH, "3");

		if (UtilImpl.CATALOG != null) {
			cfg.put(AvailableSettings.DEFAULT_CATALOG, UtilImpl.CATALOG);
		}
		if (UtilImpl.SCHEMA != null) {
			cfg.put(AvailableSettings.DEFAULT_SCHEMA, UtilImpl.SCHEMA);
		}

		// Whether to generate dynamic proxies as classes or not (adds to classes loaded and thus Permanent Generation)
		cfg.put(AvailableSettings.USE_REFLECTION_OPTIMIZER, "false");

		// Update the database schema on first use
		if (UtilImpl.DDL_SYNC) {
			cfg.put(AvailableSettings.HBM2DDL_AUTO, "update");
		}
		// The default of "grouped" may require hibernate.default_schema and/or hibernate.default_catalog to be provided.
		// Will have more luck with "individually".
		cfg.put(AvailableSettings.HBM2DDL_JDBC_METADATA_EXTRACTOR_STRATEGY, "individually");

		// Keep stats on usage
		cfg.put(AvailableSettings.GENERATE_STATISTICS, "false");

		// Log SQL to stdout
		cfg.put(AvailableSettings.SHOW_SQL, Boolean.toString(UtilImpl.SQL_TRACE));
		cfg.put(AvailableSettings.FORMAT_SQL, Boolean.toString(UtilImpl.PRETTY_SQL_OUTPUT));

		// Don't import simple class names as entity names
		cfg.put("auto-import", "false");

		StandardServiceRegistryBuilder ssrb = new StandardServiceRegistryBuilder().configure(config);
		ssrb.addService(IntegratorService.class, new IntegratorService() {
			private static final long serialVersionUID = -1078480021120121931L;

			/**
			 * Add the JPA Integrator and then the skyve event listeners.
			 */
			@Override
			public Iterable<Integrator> getIntegrators() {
				List<Integrator> result = new ArrayList<>(1);
				
				result.add(new Integrator() {
					@Override
					public void integrate(@SuppressWarnings("hiding") Metadata metadata,
											SessionFactoryImplementor sessionFactory,
											SessionFactoryServiceRegistry serviceRegistry) {
						HibernateListener listener = new HibernateListener();
						final EventListenerRegistry eventListenerRegistry = serviceRegistry.getService(EventListenerRegistry.class);

						// For Bizlet callbacks
						eventListenerRegistry.appendListeners(EventType.POST_LOAD, listener);
						eventListenerRegistry.appendListeners(EventType.PRE_DELETE, listener);
						eventListenerRegistry.appendListeners(EventType.POST_DELETE, listener);
						
						// For CMS Update callbacks
						eventListenerRegistry.appendListeners(EventType.POST_UPDATE, listener);
						eventListenerRegistry.appendListeners(EventType.POST_INSERT, listener);

						// For BizLock and BizKey callbacks
						eventListenerRegistry.appendListeners(EventType.PRE_UPDATE, listener);

						// For ordering collection elements when initialised
						eventListenerRegistry.appendListeners(EventType.INIT_COLLECTION, listener);
					}
					
					@Override
					public void disintegrate(SessionFactoryImplementor sessionFactory, SessionFactoryServiceRegistry serviceRegistry) {
						// nothing to clean up here
					}
				});
				return result;
			}
		});
		
		// NB try-with-resources fails on standardRegistry here as the standard registry is used
		// as long as the session factory is open.
		StandardServiceRegistry standardRegistry = ssrb.build();
		MetadataSources sources = new MetadataSources(standardRegistry);

		sources.addAnnotatedClass(AbstractPersistentBean.class);
		sources.addAnnotatedClass(UniquenessEntity.class);

		ProvidedRepository repository = ProvidedRepositoryFactory.get();
		if (UtilImpl.USING_JPA) {
			// cfg.configure("bizhub", null);
			// emf = javax.persistence.Persistence.createEntityManagerFactory("bizhub");
		}
		else {
			StringBuilder sb = new StringBuilder(64);

			for (String moduleName : repository.getAllVanillaModuleNames()) {
				// repository.REPOSITORY_DIRECTORY
				sb.setLength(0);
				sb.append(ProvidedRepository.MODULES_NAME).append('/');
				sb.append(moduleName).append('/');
				sb.append(ProvidedRepository.DOMAIN_NAME).append('/');
				sb.append(moduleName).append("_orm.hbm.xml");
				String mappingPath = sb.toString();

				File mappingFile = new File(UtilImpl.getAbsoluteBasePath() + mappingPath);
				if (mappingFile.exists()) {
					sources.addResource(mappingPath);
				}
			}

			// Check for customer overridden ORMs
			for (String customerName : repository.getAllCustomerNames()) {
				sb.setLength(0);
				sb.append(ProvidedRepository.CUSTOMERS_NAMESPACE).append(customerName).append('/');
				sb.append(ProvidedRepository.MODULES_NAME).append("/orm.hbm.xml");
				String ormResourcePath = sb.toString();
				
				File ormFile = new File(UtilImpl.getAbsoluteBasePath() + ormResourcePath);
				if (ormFile.exists()) {
					sources.addResource(ormResourcePath);
				}
			}
		}

		metadata = sources.getMetadataBuilder().build();
		SessionFactoryBuilder sessionFactoryBuilder = metadata.getSessionFactoryBuilder();

		bizKeyLength = ((Column) metadata.getEntityBinding(AppConstants.ADMIN_MODULE_NAME + AppConstants.CONTACT_DOCUMENT_NAME).getProperty(Bean.BIZ_KEY).getValue().getColumnIterator().next()).getLength();
		
		try {
			sf = sessionFactoryBuilder.build();
		}
		catch (CacheException e) {
			throw new IllegalStateException("A cache definition is missing from the json that is referenced in a document - see the hibernate exception below for the cache name in []", e);
		}

		if (UtilImpl.DDL_SYNC) {
			try {
				DDLDelegate.migrate(standardRegistry, metadata, AbstractHibernatePersistence.getDialect(), true);
			}
			catch (Exception e) {
				LOGGER.error("Could not apply skyve extra schema updates");
				e.printStackTrace();
			}
		}
	}

	public static @Nonnull SkyveDialect getDialect(@Nonnull String dialectClassName) {
		SkyveDialect dialect = DIALECTS.get(dialectClassName);
		if (dialect == null) {
			synchronized (AbstractHibernatePersistence.class) {
				dialect = DIALECTS.get(dialectClassName);
				if (dialect == null) {
					try {
						Class<?> dialectClass = Thread.currentThread().getContextClassLoader().loadClass(dialectClassName);
						dialect = (SkyveDialect) dialectClass.getDeclaredConstructor().newInstance();
						DIALECTS.put(dialectClassName, dialect);
					}
					catch (Exception e) {
						throw new IllegalStateException(dialectClassName + " cannot be loaded.", e);
					}
				}
			}
		}
		return dialect;
	}	
	
	public static @Nonnull SkyveDialect getDialect() {
		return getDialect(UtilImpl.DATA_STORE.getDialectClassName());
	}
	
	public static void logSecondLevelCacheStats(@Nonnull String cacheName) {
		CacheStatisticsMXBean bean = EXT.getCaching().getJCacheStatisticsMXBean(cacheName);
		if (bean != null) {
			LOGGER.info("HIBERNATE SHARED CACHE:- " + cacheName + " => " + bean.getCacheGets() + " gets : " + bean.getCachePuts() + " puts : " + bean.getCacheHits() + " hits : " + bean.getCacheMisses() + " misses : " + bean.getCacheRemovals() + " removals : " + bean.getCacheEvictions() + " evictions");
		}
	}
	
	@Override
	@SuppressWarnings("resource")
	public final void generateDDL(String dropDDLFilePath, String createDDLFilePath, String updateDDLFilePath) {
		try {
			if (dropDDLFilePath != null) {
				new SchemaExport().setOutputFile(dropDDLFilePath).drop(EnumSet.of(TargetType.SCRIPT), metadata);
			}
			if (createDDLFilePath != null) {
				new SchemaExport().setOutputFile(createDDLFilePath).createOnly(EnumSet.of(TargetType.SCRIPT), metadata);
			}
			if (updateDDLFilePath != null) {
				new SchemaUpdate().setOutputFile(updateDDLFilePath).execute(EnumSet.of(TargetType.SCRIPT), metadata);
				try (FileWriter fw = new FileWriter(updateDDLFilePath, true)) {
					try (BufferedWriter bw = new BufferedWriter(fw)) {
						for (String ddl : DDLDelegate.migrate(((MetadataImplementor) metadata).getMetadataBuildingOptions().getServiceRegistry(),
																metadata,
																AbstractHibernatePersistence.getDialect(),
																false)) {
							bw.write(ddl);
							bw.write(';');
							bw.newLine();
						}
					}
				}
			}
		}
		catch (IOException e) {
			throw new DomainException("Could not create temporary DDL file", e);
		}
		catch (Exception e) {
			throw new DomainException("Could not read temporary DDL file", e);
		}
	}

	@Override
	@SuppressWarnings("deprecation")
	public final String getDocumentEntityName(String moduleName, String documentName) {
		String overriddenEntityName = user.getCustomerName() + moduleName + documentName;

		if (sf.getMetamodel().entity(overriddenEntityName) != null) {
			return overriddenEntityName;
		}

		return moduleName + documentName;
	}
	
	private @Nonnull String getCollectionRoleName(@Nonnull String moduleName,
													@Nonnull String documentName,
													@Nonnull String collectionName) {
		return getDocumentEntityName(moduleName, documentName) + '.' + collectionName;
	}

	private void treatPersistenceThrowable(@Nonnull Throwable t,
											@Nonnull OperationType operationType,
											@Nullable PersistentBean bean) {
t.printStackTrace();
		if (t instanceof jakarta.persistence.OptimisticLockException) {
			if (bean == null) {
				throw new DomainException(t);
			}
			if (bean.isPersisted()) {
				try {
					session.refresh(bean);
				}
				catch (@SuppressWarnings("unused") Exception e) {
					// Cannot send in an entity name to refresh, so this happens when the object is transient or detached
					// So do nothing, we're about to throw Optimistic Lock anyway
				}
			}
			throw new OptimisticLockException(user, operationType, bean.getBizLock());
		}
		else if (t instanceof StaleObjectStateException) {
			if (bean == null) {
				throw new DomainException(t);
			}
			if (bean.isPersisted()) {
				try {
					session.refresh(bean);
				}
				catch (@SuppressWarnings("unused") Exception e) {
					// Cannot send in an entity name to refresh, so this happens when the object is transient or detached
					// So do nothing, we're about to throw Optimistic Lock anyway
				}
			}
			throw new OptimisticLockException(user, operationType, bean.getBizLock());
		}
		else if (t instanceof EntityNotFoundException) {
			if (bean == null) {
				throw new DomainException(t);
			}
			throw new OptimisticLockException(user, operationType, bean.getBizLock());
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
			if (UtilImpl.DEV_MODE) {
				if (t instanceof QuerySyntaxException) {
					String m = t.getMessage();
					if ((m != null) && m.contains("is not mapped")) {
						throw new DomainException("Entity is not mapped.  Is domain generation required or are there compile errors?", t);
					}
				}
			}
			throw new DomainException(t);
		}
	}

	@Override
	public final void begin() {
		try {
			EntityTransaction et = em.getTransaction();
			if (! et.isActive()) {
				// FROM THE HIBERNATE_REFERENCE DOCS Page 190
	            // Earlier versions of Hibernate required explicit disconnection and reconnection of a Session. 
	            // These methods are deprecated, as beginning and ending a transaction has the same effect.
				et.begin();
			}
		}
		finally {
			if (dynamicPersistence != null) {
				dynamicPersistence.begin();
			}
		}
	}

	@Override
	public void setUser(User user) {
		super.setUser(user);
		resetDocumentPermissionScopes();
	}

	@Override
	public <R> R withDocumentPermissionScopes(DocumentPermissionScope scope, Function<Persistence, R> function) {
		try {
			setDocumentPermissionScopes(scope);
			return function.apply(this);
		}
		finally {
			resetDocumentPermissionScopes();
		}
	}

	@Override
	public void withDocumentPermissionScopes(DocumentPermissionScope scope, Consumer<Persistence> consumer) {
		try {
			setDocumentPermissionScopes(scope);
			consumer.accept(this);
		}
		finally {
			resetDocumentPermissionScopes();
		}
	}
	
	private void setDocumentPermissionScopes(@Nonnull DocumentPermissionScope scope) {
		Set<String> accessibleModuleNames = ((UserImpl) user).getAccessibleModuleNames(); 
		ProvidedRepository repository = ProvidedRepositoryFactory.get();

		// Enable all filters required for this user
		for (String moduleName : repository.getAllVanillaModuleNames()) {
			Customer moduleCustomer = (accessibleModuleNames.contains(moduleName) ? user.getCustomer() : null);
			Module module = repository.getModule(moduleCustomer, moduleName);

			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(moduleCustomer, documentName);
				if ((! document.isDynamic()) && // is not dynamic
						document.isPersistable() && // is persistent document
						(repository.findNearestPersistentUnmappedSuperDocument(moduleCustomer, module, document) == null) && // not a sub-class (which don't have filters)
						moduleName.equals(document.getOwningModuleName())) { // document belongs to this module
					setFilters(document, scope);
				}
			}
		}
	}
	
	private void resetDocumentPermissionScopes() {
		Set<String> accessibleModuleNames = user.getAccessibleModuleNames(); 
		ProvidedRepository repository = ProvidedRepositoryFactory.get();

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
				if ((! document.isDynamic()) && // is not dynamic
						(document.isPersistable()) && // is persistent document
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
	private void setFilters(@Nonnull Document document, @Nonnull DocumentPermissionScope scope) {
		Set<String> accessibleModuleNames = ((UserImpl) user).getAccessibleModuleNames(); 
		ProvidedRepository repository = ProvidedRepositoryFactory.get();
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
		
		// Catch HibernateException here which may mean that gen domain needs to be run.
		try {
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
			// Only apply the customer filter if we are in multi-tenant mode
			if (UtilImpl.CUSTOMER == null) {
				if (DocumentPermissionScope.customer.equals(scope) ||
						DocumentPermissionScope.dataGroup.equals(scope) ||
						DocumentPermissionScope.user.equals(scope)) {
					Filter filter = session.enableFilter(customerFilterName);
					filter.setParameter("customerParam", customer.getName());
				}
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
		catch (HibernateException e) {
			if (UtilImpl.DEV_MODE) {
				throw new DomainException("Unable to set hibernate filters - Is domain generation required or are there compile errors?", e);
			}
			throw e;
		}
	}
	
	/**
	 * Reset filters to the document default
	 * 
	 * @param document
	 * @param scope
	 */
	private void resetFilters(@Nonnull Document document) {
		DocumentPermissionScope scope = user.getScope(document.getOwningModuleName(), document.getName());
		setFilters(document, scope);
	}
	
	@Override
	public final void setRollbackOnly() {
		if (em != null) {
			EntityTransaction et = em.getTransaction();
			if ((et != null) && et.isActive()) {
				et.setRollbackOnly();
			}
		}
	}
	
	// This code is called in exception blocks all over the place.
	// So we have to ensure its robust as all fuck
	@Override
	public final void rollback() {
		boolean rollbackOnly = false;
		try {
			if (em != null) {
				EntityTransaction et = em.getTransaction();
				if ((et != null) && et.isActive()) {
					rollbackOnly = et.getRollbackOnly();
					if (! rollbackOnly) {
						// FROM THE HIBERNATE_REFERENCE DOCS Page 190
		                // Earlier versions of Hibernate required explicit disconnection and reconnection of a Session. 
		                // These methods are deprecated, as beginning and ending a transaction has the same effect.
						et.rollback();
					}
				}
			}
		}
		finally {
			try {
				if ((! rollbackOnly) && (dynamicPersistence != null)) {
					dynamicPersistence.rollback();
				}
			}
			finally {
				try {
					uniqueBeansChecked.clear();
				}
				finally {
					uniqueHashes.clear();
				}
			}				
		}
	}

	// This code is called in finally blocks all over the place.
	// So we have to ensure its robust as all fuck
	@Override
	public final void commit(boolean close) {
		commit(close, true);
	}
	
	// this variant is used by BackupUtil.executeScript() where the ADM_)Uniqueness table 
	// may not exist after a script - eg drop script
	public final void commit(boolean close, boolean removeUniqueHashes) {
		boolean rollbackOnly = false;
		try {
			if (em != null) { // can be null after a relogin
				EntityTransaction et = em.getTransaction();
				if ((et != null) && et.isActive()) {
					rollbackOnly = et.getRollbackOnly();
					if (rollbackOnly) {
		                // FROM THE HIBERNATE_REFERENCE DOCS Page 190
		                // Earlier versions of Hibernate required explicit disconnection and reconnection of a Session. 
		                // These methods are deprecated, as beginning and ending a transaction has the same effect.
						et.rollback();
					}
					else {
						// Remove all inserted unique hashes if we were told to (can only do if we have an em)
						// If we cannot, rollback, and continue closing resources
						if (removeUniqueHashes) {
							try {
								final Persistent persistent = new Persistent();
								persistent.setName(UniquenessEntity.TABLE_NAME);
								final String persistentIdentifier = persistent.getPersistentIdentifier();
								for (String hash : uniqueHashes) {
									StringBuilder query = new StringBuilder(64);
									query.append("delete from ").append(persistentIdentifier).append(" where ");
									query.append(UniquenessEntity.HASH_COLUMN_NAME).append(" = :").append(UniquenessEntity.HASH_COLUMN_NAME);
									newSQL(query.toString()).putParameter(UniquenessEntity.HASH_COLUMN_NAME, hash, false).execute();
								}
							}
							// Note if we can't remove the hashes here that could screw every subsequent
							// persistence operation on the server if we were to commit, so rollback and chuck.
							catch (Exception e) {
								// Set this for dynamic persistence treatment below
								rollbackOnly = true;

								LOGGER.error("Cannot remove unique hashes (stack trace underneath) - attempting a rollback....", e);
								e.printStackTrace();

								// try rollback
								et.rollback();
								// keep throwing
								throw e;
							}
						}

						// Commit only if no error above
						// FROM THE HIBERNATE_REFERENCE DOCS Page 190
						// Earlier versions of Hibernate required explicit disconnection and reconnection of a Session. 
						// These methods are deprecated, as beginning and ending a transaction has the same effect.
						et.commit();
					}
				}
			}
		}
		catch (@SuppressWarnings("unused") RollbackException e) {
			LOGGER.warn("Cannot commit as transaction was rolled back earlier....");
		}
		finally {
			try {
				if (dynamicPersistence != null) {
					if (rollbackOnly) {
						dynamicPersistence.rollback();
					}
					else {
						dynamicPersistence.commit();
					}
				}
			}
			finally {
				try {
					closeContent();
				}
				catch (Exception e) {
					LOGGER.warn("Cannot commit content manager - {}", e.getLocalizedMessage(), e);
					e.printStackTrace();
				}
				finally {
					try {
						try {
							uniqueBeansChecked.clear();
						}
						finally {
							uniqueHashes.clear();
						}
					}
					finally {
						if (close) {
							close();
							threadLocalPersistence.remove();
						}
					}
				}
			}
		}
	}

	public void close() {
		try {
			if (em != null) { // can be null after a relogin
				em.close();
			}
			em = null;
			session = null;
		}
		finally {
			if (dynamicPersistence != null) {
				dynamicPersistence.close();
			}
		}
	}
	
	@Override
	public void evictAllCached() {
		session.clear();
		dynamicPersistence.evictAllCached();
	}

	@Override
	public void evictCached(Bean bean) {
		if (bean instanceof DynamicBean) {
			dynamicPersistence.evictCached(bean);
		}
		else if (cached(bean)) {
			session.evict(bean);
		}
	}
	
	@Override
	public boolean cached(Bean bean) {
		if (bean instanceof DynamicBean) {
			return dynamicPersistence.cached(bean);
		}
		return session.contains(getDocumentEntityName(bean.getBizModule(), bean.getBizDocument()), bean);
	}
	
	@Override
	public boolean sharedCacheCollection(String moduleName, String documentName, String collectionName, String ownerBizId) {
		String role = getCollectionRoleName(moduleName, documentName, collectionName);
		return sf.getCache().containsCollection(role, ownerBizId);
	}
	
	@Override
	public boolean sharedCacheCollection(Bean owner, String collectionName) {
		return sharedCacheCollection(owner.getBizModule(), owner.getBizDocument(), collectionName, owner.getBizId());
	}
	
	@Override
	public boolean sharedCacheBean(String moduleName, String documentName, String bizId) {
		return sf.getCache().containsEntity(getDocumentEntityName(moduleName, documentName), bizId);
	}

	@Override
	public boolean sharedCacheBean(Bean bean) {
		return sharedCacheBean(bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
	}
	
	@Override
	public void evictAllSharedCache() {
		sf.getCache().evictAllRegions();
	}
	
	@Override
	public void evictSharedCacheCollections() {
		sf.getCache().evictCollectionData();
	}
	
	@Override
	public void evictSharedCacheCollections(String moduleName, String documentName, String collectionName) {
		String role = getCollectionRoleName(moduleName, documentName, collectionName);
		sf.getCache().evictCollectionData(role);
	}
	
	@Override
	public void evictSharedCacheCollection(String moduleName, String documentName, String collectionName, String ownerBizId) {
		String role = getCollectionRoleName(moduleName, documentName, collectionName);
		sf.getCache().evictCollectionData(role, ownerBizId);
	}
	
	@Override
	public void evictSharedCacheCollection(Bean owner, String collectionName) {
		evictSharedCacheCollection(owner.getBizModule(), owner.getBizDocument(), collectionName, owner.getBizId());
	}
	
	@Override
	public void evictSharedCacheBeans() {
		sf.getCache().evictEntityData();
	}
	
	@Override
	public void evictSharedCacheBeans(String moduleName, String documentName) {
		sf.getCache().evictEntityData(getDocumentEntityName(moduleName, documentName));
	}
	
	@Override
	public void evictSharedCachedBean(String moduleName, String documentName, String bizId) {
		sf.getCache().evictEntityData(getDocumentEntityName(moduleName, documentName), bizId);
	}
	
	@Override
	public void evictSharedCachedBean(Bean bean) {
		evictSharedCachedBean(bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
	}

	/**
	 * The refresh method is not on the Persistence interface as Hibernate can
	 * call et.setRollbackOnly() when exceptions are thrown by session.refresh().
	 * Use this with caution - if et.setRollbackOnly() is called there is no way to undo it
	 * and any calls to Persistence.commit() will just not work.
	 * @param bean
	 */
	public void refresh(@Nonnull Bean bean) {
		if (bean.isPersisted()) {
			try {
				session.refresh(bean);
			}
			catch (MappingException | IllegalArgumentException e) {
				// Cannot send in an entity name to refresh, so this happens when the object is transient or detached
				// So do nothing, we're about to throw Optimistic Lock anyway
				throw new DomainException("Bean " + bean.toString() + " is transient or detached", e);
			}
		}
	}

	@Override
	public void flush() {
		em.flush();
	}
	
	// populate all implicit mandatory fields required
	private void setMandatories(@Nonnull Document document,
									final @Nonnull PersistentBean beanToSave,
									@Nullable Map<PersistentBean, PersistentBean> beansToMerge) {
		final Customer customer = user.getCustomer();

		new BeanVisitor(true, false) {
			@Override
			@SuppressWarnings("synthetic-access")
			protected boolean accept(String binding,
										@SuppressWarnings("hiding") Document document,
										Document owningDocument,
										Relation owningRelation,
										Bean bean) 
			throws Exception {
				// Process an inverse if the inverse is specified as cascading.
				if ((owningRelation instanceof Inverse) && 
						(! Boolean.TRUE.equals(((Inverse) owningRelation).getCascade()))) {
					return false;
				}
			
				if (document.isPersistable()) {
					// dataGroup and user are NOT set here - it is only set on newInstance()
					// this allows us to set the data group programmatically.
					// DataGroup and user are used to create a path
					// for content and so can not change.
					// 
					// Content should be saved in the 1 workspace, this means bizCustomer is not syched a massive security risk.
					// Customer is not the broadest scope - global is, so customer data can be interrelated at the customer level.
					// eg bizhub could maintain global post codes for all its customers, who may link to it, but when the other
					// customers save their data, we do not want them taking ownership of our postcodes.
//					bean.setBizCustomer(customer.getName());

					// We set the bizKey unconditionally as the bizKey may be dependent on child or related
					// beans and not just on properties in this bean.
					// That means we can't rely on preUpdate event listener as preUpdate may not get fired if this 
					// bean hasn't changed, but the related dependent bean has changed requiring the update to bizKey.
					PersistentBean persistentBean = (PersistentBean) bean;
					document.setBizKey(persistentBean);

					// This only sets the bizLock if the bean is about to be inserted
					// If we set it here on update, we are making the bean dirty even if there
					// are no actual changes made to it.
					// The HibernateListener is used to generate bizLock on update.
					// Note:- This stops hibernate metadata validation from moaning
					if (! bean.isPersisted()) {
						persistentBean.setBizLock(new OptimisticLock(user.getName(), new Date()));

						// Add non-dynamic (static) unpersisted (transient) beans reachable by persistent dynamic relations to the Set to persist later (excluding top-level).
						// This implements persistence by reachability for dynamic -> static beans in mixed graphs
						if (beansToMerge != null) {
							if ((owningRelation != null) && // not the top-level bean or parent
									(owningDocument != null) && // not the top-level bean or parent
									(persistentBean != beanToSave) && // not a reference to the top level bean
									(! document.isDynamic()) && // bean is not dynamic
										owningRelation.isPersistent() && // persistent relation
										owningDocument.isDynamic()) { // dynamic relation
								beansToMerge.put(persistentBean, null);
							}
						}
					}
				}

				return true;
			}
		}.visit(document, beanToSave, customer);
	}
	
	@Override
	public void preMerge(Document document, PersistentBean beanToSave) {
		preMerge(document, beanToSave, null);
	}
	
	private void preMerge(@Nonnull Document document,
							@Nonnull PersistentBean beanToSave,
							@Nullable Map<PersistentBean, PersistentBean> beansToMerge) {
		// set bizCustomer, bizLock & bizKey
		setMandatories(document, beanToSave, null);
		
		// Validate all and sundry before touching the database
		// Note that preSave events are all fired for the object graph and then validate for the graph is called.
		// This allows preSave events to mutate values and build more object graph nodes on before it is all validated.
		Customer customer = user.getCustomer();
		firePreSaveEvents(customer, document, beanToSave);
		validatePreMerge(customer, document, beanToSave);

		// set bizCustomer, bizLock & bizKey again in case more object hierarchy has been added during preSave()
		setMandatories(document, beanToSave, beansToMerge);
	}

	private static void firePreSaveEvents(@Nonnull final Customer customer,
											@Nonnull Document document,
											@Nonnull final Bean beanToSave) {
		new BeanVisitor(true, false) {
			@Override
			protected boolean accept(String binding,
										@SuppressWarnings("hiding") Document document,
										Document owningDocument,
										Relation owningRelation,
										Bean bean)
			throws Exception {
				// Process an inverse if the inverse is specified as cascading.
				if ((owningRelation instanceof Inverse) && 
						(! Boolean.TRUE.equals(((Inverse) owningRelation).getCascade()))) {
					return false;
				}
				
				// Persistent references will persist non-persisted (but persistent) beans, or cascade persisted beans.
				// Transient references will NOT persist non-persisted (but persistent) beans, but changes are cascaded (by hibernate).
				// We could have a transient reference to a persisted bean and 
				// the save operation still needs to cascade persist any changes to the 
				// persistent attributes in the referenced document.
				try {
					Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);

					// If it is the top-level bean or parent bean it is of course persistent since we are in preSave() here
					// If bean is persisted, call preSave as changes will be flushed
					// If bean is transient, it will be persisted by reachability by hibernate if it is a persistent reference
					if ((owningRelation == null) || bean.isPersisted() || owningRelation.isPersistent()) {
						CustomerImpl internalCustomer = (CustomerImpl) customer;
						boolean vetoed = internalCustomer.interceptBeforePreSave(bean);
						if (! vetoed) {
							if (bizlet != null) {
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering " + bizlet.getClass().getName() + ".preSave: " + bean);
								bizlet.preSave(bean);
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting " + bizlet.getClass().getName() + ".preSave");
							}
							internalCustomer.interceptAfterPreSave(bean);
						}
					}
				}
				catch (ValidationException e) {
					for (Message message : e.getMessages()) {
						ValidationUtil.processMessageBindings(customer, message, beanToSave, bean);
					}
					throw e;
				}

				return true;
			}
		}.visit(document, beanToSave, customer);
	}
	
	private void validatePreMerge(@Nonnull final Customer customer,
									@Nonnull Document document,
									@Nonnull final Bean beanToSave) {
		new BeanVisitor(true, false) {
			@Override
			protected boolean accept(String binding,
										@SuppressWarnings("hiding") Document document,
										Document owningDocument,
										Relation owningRelation,
										Bean bean)
			throws Exception {
				// Process an inverse if the inverse is specified as cascading.
				if ((owningRelation instanceof Inverse) && 
						(! Boolean.TRUE.equals(((Inverse) owningRelation).getCascade()))) {
					return false;
				}
				
				try {
					ValidationUtil.validateBeanAgainstDocument(document, bean);

					Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
					if (bizlet != null) { // has a bizlet
						ValidationUtil.validateBeanAgainstBizlet(bizlet, bean);
					}

					ValidationUtil.checkCollectionUniqueConstraints(customer, document, bean);

					// NOTE:- We only check if the document is a persistent document here,
					// not if the reference (if any) is persistent.
					// We could have a transient reference to a persistent document and 
					// the save operation still needs to cascade persist any changes to the 
					// persistent attributes in the referenced document.
					if (document.isPersistable()) {
						if (owningRelation == null) { // top level or parent binding
							checkUniqueConstraints(customer, document, bean);
						}
						else {
							boolean persistentRelation = owningRelation.isPersistent();
							// Don't check the unique constraints if the relation is not persistent
							// and the instance will not be persisted by reachability - ie the bean is transient too
							if (persistentRelation || bean.isPersisted()) {
								checkUniqueConstraints(customer, document, bean);
							}
						}
						
						// Re-evaluate the bizKey after all events have fired
						// as the bizKey may be dependent on values that have mutated  
						// NB - We do unconditionally as the bizKey may be dependent on child or related
						// beans and not just on properties in this bean.
						// That means we can't rely on preUpdate event listener as preUpdate may not get fired if this 
						// bean hasn't changed, but the related dependent bean has changed requiring the update to bizKey.
						document.setBizKey((PersistentBean) bean);
					}
				}
				catch (ValidationException e) {
					for (Message message : e.getMessages()) {
						ValidationUtil.processMessageBindings(customer, message, beanToSave, bean);
					}
					throw e;
				}

				return true;
			}
		}.visit(document, beanToSave, customer);
	}
	
	@Override
	public final <T extends PersistentBean> T save(Document document, T bean) {
		return save(document, bean, true);
	}
	
	@Override
	public final <T extends PersistentBean> T merge(Document document, T bean) {
		return save(document, bean, false);
	}

	@Override
	public final <T extends PersistentBean> List<T> save(@SuppressWarnings("unchecked") T... beans) {
		return save(Arrays.asList(beans), true);
	}
	
	@Override
	public final <T extends PersistentBean> List<T> save(List<T> beans) {
		return save(beans, true);
	}

	@Override
	public final <T extends PersistentBean> List<T> merge(@SuppressWarnings("unchecked") T... beans) {
		return save(Arrays.asList(beans), false);
	}
	
	@Override
	public final <T extends PersistentBean> List<T> merge(List<T> beans) {
		return save(beans, false);
	}

	/**
	 * This is a stack so that save operations called in preSave Bizlet methods will work correctly.
	 * 
	 * Non-dynamic (static/generated) beans that are not persisted and are reachable by persistent dynamic references
	 * need to be merged and have the new hibernate object replace any transient references in the bean graph.
	 * 
	 * The top-level bean is not added to this set.
	 * 
	 * A new context is pushed onto the stack at the beginning of a save(T) or save(T...) operation.
	 * This stack is popped and the Map variable is cleared at the end of the save operation.
	 * See the finally blocks below.
	 * 
	 * The set of beans is merged in the guts of the save(T) or save(T...) methods.
	 * This is a Map of the unmerged beans found in preMerge() that should be persisted to null.
	 * Once they are merged, the merged bean is placed against each unmerged bean.
	 */
	private Stack<Map<PersistentBean, PersistentBean>> saveContext = new Stack<>();

	@SuppressWarnings("unchecked")
	private @Nonnull <T extends PersistentBean> T save(@Nonnull Document document, @Nonnull T bean, boolean flush) {
		T result = null;
		
		Map<PersistentBean, PersistentBean> beansToMerge = null;
		
		try {
			CustomerImpl internalCustomer = (CustomerImpl) getUser().getCustomer();
			boolean vetoed = false;
			
			// We need to replace transient properties before calling postMerge as
			// Bizlet.postSave() implementations could manipulate these transients for display after save.
			try {
				vetoed = internalCustomer.interceptBeforeSave(document, bean);
				if (! vetoed) {
					// Start a new save context
					beansToMerge = new LinkedHashMap<>();
					saveContext.push(beansToMerge);
					
					preMerge(document, bean, beansToMerge);

					// Merge the bean to save
					if (document.isDynamic()) {
						result = bean;
					}
					else {
						String entityName = getDocumentEntityName(document.getOwningModuleName(), document.getName());
						result = (T) session.merge(entityName, bean);
					}
					
					// Persist (by reachability) static beans referenced by dynamic relations found in preMerge()
					for (PersistentBean beanToMerge : beansToMerge.keySet()) {
						String entityName = getDocumentEntityName(beanToMerge.getBizModule(), beanToMerge.getBizDocument());
						try {
							beansToMerge.put(beanToMerge, (PersistentBean) session.merge(entityName, beanToMerge));
						}
						// if we cant merge any bean, act like we didn't merge the main bean
						catch (Throwable t) {
							result = null;
							throw t;
						}
					}
					
					if (flush) {
						em.flush();
					}
				}
			}
			finally {
				if (result != null) { // only do if we got a result from the merge
					if (beansToMerge != null) { // shut the compiler up here
						beansToMerge.put(bean, result);
						// Prepare all merged beans (reinstate non-persistents and re-point to persistent instances)
						for (Entry<PersistentBean, PersistentBean> entry : beansToMerge.entrySet()) {
							PersistentBean beanToMerge = entry.getKey();
							PersistentBean mergedBean = entry.getValue();
							if (mergedBean != null) { // only do if we got a result from the merge
								Module m = internalCustomer.getModule(mergedBean.getBizModule());
								Document d = m.getDocument(internalCustomer, mergedBean.getBizDocument());
								prepareMergedBean(d, mergedBean, beanToMerge, beansToMerge);
							}
						}
					}
				}
			}

			if (result != null) {
				if (! vetoed) {
					// Flush dynamic domain
					if ((document.getPersistent() != null) && document.hasDynamic()) { // persistent (somehow) with dynamism somewhere
						dynamicPersistence.persist(result);
					}
	
					postMerge(document, result);
					internalCustomer.interceptAfterSave(document, result);
				}
			}
		}
		catch (Throwable t) {
			treatPersistenceThrowable(t, OperationType.update, bean);
		}
		finally {
			if (beansToMerge != null) { // save context was pushed
				beansToMerge.clear();
				saveContext.pop();
			}
		}

		if (result == null) {
			result = bean;
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	private @Nonnull <T extends PersistentBean> List<T> save(@Nonnull List<T> beans, boolean flush) {
		List<T> results = new ArrayList<>();
		PersistentBean currentBean = null; // used in exception handling

		Map<PersistentBean, PersistentBean> beansToMerge = null;

		try {
			CustomerImpl internalCustomer = (CustomerImpl) getUser().getCustomer();
			boolean vetoed = false;
			
			// We need to replace transient properties before calling postMerge as
			// Bizlet.postSave() implementations could manipulate these transients for display after save.
			try {
				// fire any interceptors before any other processing as these are being treated like a batch
				for (PersistentBean bean : beans) {
					currentBean = bean; // for exception handling
					Module m = internalCustomer.getModule(bean.getBizModule());
					Document d = m.getDocument(internalCustomer, bean.getBizDocument());
					vetoed = internalCustomer.interceptBeforeSave(d, bean);
					if (vetoed) {
						break;
					}
				}
				if (! vetoed) {
					// Start a new save context
					beansToMerge = new LinkedHashMap<>();
					saveContext.push(beansToMerge);
					
					for (PersistentBean bean : beans) {
						currentBean = bean; // for exception handling
						Module m = internalCustomer.getModule(bean.getBizModule());
						Document d = m.getDocument(internalCustomer, bean.getBizDocument());
						preMerge(d, bean, beansToMerge);
					}
					
					// Merge the beans to save
					for (PersistentBean bean : beans) {
						currentBean = bean; // for exception handling
						Module m = internalCustomer.getModule(bean.getBizModule());
						Document d = m.getDocument(internalCustomer, bean.getBizDocument());

						if (d.isDynamic()) {
							results.add((T) bean);
						}
						else {
							String entityName = getDocumentEntityName(d.getOwningModuleName(), d.getName());
							results.add((T) session.merge(entityName, bean));
						}
					}
					
					// Persist (by reachability) static beans referenced by dynamic relations found in preMerge()
					for (PersistentBean beanToMerge : beansToMerge.keySet()) {
						String entityName = getDocumentEntityName(beanToMerge.getBizModule(), beanToMerge.getBizDocument());
						try {
							beansToMerge.put(beanToMerge, (PersistentBean) session.merge(entityName, beanToMerge));
						}
						// if we can't merge any bean, act like we didn't merge the main beans
						catch (Throwable t) {
							results.clear();
							throw t;
						}
					}

					if (flush) {
						em.flush();
					}
				}
			}
			finally {
				if (beansToMerge != null) { // shut the compiler up here
					int i = 0;
					// Add results to the beansToMerge
					for (PersistentBean result : results) {
						if (result != null) { // only do if we got a result from the merge
							PersistentBean bean = beans.get(i);
							beansToMerge.put(bean, result);
						}
						i++;
					}
					// Prepare all merged beans (reinstate non-persistents and re-point to persistent instances)
					for (Entry<PersistentBean, PersistentBean> entry : beansToMerge.entrySet()) {
						PersistentBean beanToMerge = entry.getKey();
						PersistentBean mergedBean = entry.getValue();
						if (mergedBean != null) { // only do if we got a result from the merge
							Module m = internalCustomer.getModule(mergedBean.getBizModule());
							Document d = m.getDocument(internalCustomer, mergedBean.getBizDocument());
							prepareMergedBean(d, mergedBean, beanToMerge, beansToMerge);
						}
					}
				}
			}
			if (! vetoed) {
				// Flush dynamic domain
				for (PersistentBean result : results) {
					currentBean = result; // for exception handling
					Module m = internalCustomer.getModule(result.getBizModule());
					Document d = m.getDocument(internalCustomer, result.getBizDocument());
					if ((d.getPersistent() != null) && d.hasDynamic()) { // persistent (somehow) with dynamism somewhere
						dynamicPersistence.persist(result);
					}
				}
				
				// Post merge
				for (PersistentBean result : results) {
					currentBean = result; // for exception handling
					Module m = internalCustomer.getModule(result.getBizModule());
					Document d = m.getDocument(internalCustomer, result.getBizDocument());
					postMerge(d, result);
				}
				
				// Fire interceptors
				for (PersistentBean result : results) {
					currentBean = result; // for exception handling
					Module m = internalCustomer.getModule(result.getBizModule());
					Document d = m.getDocument(internalCustomer, result.getBizDocument());
					internalCustomer.interceptAfterSave(d, result);
				}
			}
		}
		catch (Throwable t) {
			treatPersistenceThrowable(t, OperationType.update, currentBean);
		}
		finally {
			if (beansToMerge != null) { // save context was pushed
				beansToMerge.clear();
				saveContext.pop();
			}
		}

		return results;
	}
	
	@Override
	public void postMerge(Document document, final PersistentBean beanToSave) {
		final Customer customer = user.getCustomer();
		
		new BeanVisitor(true, false) {
			@Override
			protected boolean accept(String binding,
										@SuppressWarnings("hiding") Document document,
										Document owningDocument,
										Relation owningRelation,
										Bean bean) 
			throws Exception {
				// Process an inverse if the inverse is specified as cascading.
				if ((owningRelation instanceof Inverse) && 
						(! Boolean.TRUE.equals(((Inverse) owningRelation).getCascade()))) {
					return false;
				}
				
				// persisted for this bean graph
				if (bean.isPersisted()) {
					try {
						CustomerImpl internalCustomer = (CustomerImpl) customer;
						boolean vetoed = internalCustomer.interceptBeforePostSave(bean);
						if (! vetoed) {
							Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
							if (bizlet != null) {
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering " + bizlet.getClass().getName() + ".postSave: " + bean);
								bizlet.postSave(bean);
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting " + bizlet.getClass().getName() + ".postSave");
							}
							internalCustomer.interceptAfterPostSave(bean);
						}
					}
					catch (ValidationException e) {
						for (Message message : e.getMessages()) {
							ValidationUtil.processMessageBindings(customer, message, beanToSave, bean);
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

	private void prepareMergedBean(@Nonnull Document document,
									@Nonnull final PersistentBean mergedBean,
									@Nullable final PersistentBean unmergedBean,
									@Nonnull Map<PersistentBean, PersistentBean> otherMergedBeans) {
		Customer customer = user.getCustomer();

		new BeanVisitor(true, false) {
			@Override
			protected boolean accept(String binding,
										@SuppressWarnings("hiding") Document document,
										Document owningDocument,
										Relation owningRelation,
										Bean unmergedPart) {
				// Replace any bean that has been persisted by reachability but is the old unpersisted/detached version
				Bean mergedPart = binding.isEmpty() ? mergedBean : (Bean) BindUtil.get(mergedBean, binding);
				if (mergedPart == null) { // when a dynamic relation encountered and not persisted
					BindUtil.set(mergedBean, binding, unmergedPart);
					mergedPart = unmergedPart;
				}
				else if (! binding.isEmpty()) { // not top level bean
					// Set any deeper references to the top level unmerged bean to the top level merged bean
					if ((unmergedPart == unmergedBean) && // a deeper reference to the top level bean (unmerged)
							(unmergedPart != mergedBean)) { // but the reference is not the merged bean
						BindUtil.set(mergedBean, binding, mergedBean);
						mergedPart = mergedBean;
					}
					// Set any references that were merged by persistence-by-reachability
					else if (otherMergedBeans.containsKey(unmergedPart)) {
						PersistentBean otherMergedBean = otherMergedBeans.get(unmergedPart);
						if (unmergedPart != otherMergedBean) {
							BindUtil.set(mergedBean, binding, otherMergedBean);
							mergedPart = otherMergedBean;
						}
					}
				}
				
				// Process an inverse only if the inverse is specified as cascading.
				if ((owningRelation instanceof Inverse) && 
						(! Boolean.TRUE.equals(((Inverse) owningRelation).getCascade()))) {
					return false;
				}
				
				if (mergedPart != null) {
					// Reinstate any "biz" attributes lost when detached or persisted for the first time (some "biz" attributes are not used when embedded)
					String bizCustomer = mergedPart.getBizCustomer();
					if (bizCustomer == null) {
						mergedPart.setBizCustomer(unmergedPart.getBizCustomer());
					}
					String bizDataGroupId = mergedPart.getBizDataGroupId();
					if (bizDataGroupId == null) {
						mergedPart.setBizDataGroupId(unmergedPart.getBizDataGroupId());
					}
					String bizUserId = mergedPart.getBizUserId();
					if (bizUserId == null) {
						mergedPart.setBizUserId(unmergedPart.getBizUserId());
					}
					if (mergedPart instanceof PersistentBean) {
						PersistentBean unmergedPersistentBean = (PersistentBean) unmergedPart;
						PersistentBean mergedPersistentBean = (PersistentBean) mergedPart;
						String bizKey = mergedPersistentBean.getBizKey();
						if (bizKey == null) {
							mergedPersistentBean.setBizKey(unmergedPersistentBean.getBizKey());
						}
						String bizFlagComment = mergedPersistentBean.getBizFlagComment();
						if (bizFlagComment == null) {
							mergedPersistentBean.setBizFlagComment(unmergedPersistentBean.getBizFlagComment());
						}
					}
					
					// Reinstate the transient attributes in the mergedBean from the unmerged bean lost when detached or persisted for the first time.
					Module module = customer.getModule(document.getOwningModuleName());
					for (Attribute attribute : document.getAllAttributes(customer)) {
						String attributeName = attribute.getName();
						
						boolean dynamic = BindUtil.isDynamic(customer, module, document, attribute);
						if ((dynamic || (! attribute.isPersistent())) && (! Bean.BIZ_KEY.equals(attributeName))) {
							if (attribute instanceof Collection) {
								@SuppressWarnings("unchecked")
								List<Bean> mergedCollection = (List<Bean>) BindUtil.get(mergedPart, attributeName);
								@SuppressWarnings("unchecked")
								List<Bean> unmergedCollection = (List<Bean>) BindUtil.get(unmergedPart, attributeName);
	
								// ensure that we do not try to add the elements of a collection to itself
								if ((mergedCollection != null) && 
										(unmergedCollection != null) &&
										(mergedCollection != unmergedCollection)) {
									mergedCollection.clear();
									mergedCollection.addAll(unmergedCollection);
								}
							}
							else {
								BindUtil.set(mergedPart, attributeName, BindUtil.get(unmergedPart, attributeName));
							}
						}
					}
				}
				
				return true;
			}
		}.visit(document, unmergedBean, customer);
	}

	// A set of unique constraint hashes from when a transaction begins until it commits or rolls back.
	// This set of hashes is inserted in ADM_Uniqueness (temporarily) to allow database locking between transactions.
	private Set<String> uniqueHashes = new TreeSet<>();
	
	// A set of unique beans (bizID unique) from when a transaction begins until it commits or rolls back.
	// This ensures we don't run unique constraint checking on the same bean in vararg save() calls.
	private Set<Bean> uniqueBeansChecked = new TreeSet<>();
	
	/**
	 * Check the unique constraints for a document bean.
	 * 
	 * @param customer
	 * @param document
	 * @param bean
	 */
	private void checkUniqueConstraints(@Nonnull Customer customer, @Nonnull Document document, @Nonnull Bean bean) {
// TODO - Work the dynamic something in here - remove the short-circuit on dynamic
if (document.isDynamic()) return;

		final String owningModuleName = document.getOwningModuleName();
		final Module owningModule = customer.getModule(owningModuleName);
		final String documentName = document.getName();
		final String entityName = getDocumentEntityName(owningModuleName, documentName);
		final boolean persisted = isPersisted(bean);
		// Don't check for insert uniqueness if we've already checked this instance
		final boolean insertUniquenessRequiredButNotChecked = (! persisted) && uniqueBeansChecked.add(bean);

		try {
			for (UniqueConstraint constraint : document.getAllUniqueConstraints(customer)) {
				DocumentScope scope = constraint.getScope();
				
				// Don't check unique constraints if any of the parameters is null
				boolean nullParameter = false;

				// Calculate a hash of the unique key and place into the uniqueHashes Set.
				// If the hash is inserted twice then we have a unique constraint violation within the same transaction.
				// To detect unique constraints between simultaneous transactions, we insert into ADM_Uniqueness.
				// This will lock any transaction with the same hash until the commit of the first transaction.
				// At this point the normal read lock unique constraint check will find the freshly inserted duplicate.
				// Note that these rows are deleted just before commit of the transaction to release the locks
				// and nothing is actually ever committed to this table.
				if (insertUniquenessRequiredButNotChecked) {
					StringBuilder uniqueKey = new StringBuilder(128);
					uniqueKey.append(document.getOwningModuleName()).append('|').append(document.getName()).append('|');
					if (DocumentScope.customer.equals(scope)) {
						uniqueKey.append(bean.getBizCustomer()).append('|');
					}
					else if (DocumentScope.dataGroup.equals(scope)) {
						uniqueKey.append(bean.getBizCustomer()).append('|');
						uniqueKey.append(bean.getBizDataGroupId()).append('|');
					}
					else if (DocumentScope.user.equals(scope)) {
						uniqueKey.append(bean.getBizCustomer()).append('|');
						uniqueKey.append(bean.getBizDataGroupId()).append('|');
						uniqueKey.append(bean.getBizUserId()).append('|');
					}
					for (String fieldName : constraint.getFieldNames()) {
						Object constraintFieldValue = null;
						try {
							constraintFieldValue = BindUtil.get(bean, fieldName);
						}
						catch (Exception e) {
							throw new DomainException(e);
						}
						
						// Don't do the constraint check if any query parameter is null
						if (constraintFieldValue == null) {
							if (UtilImpl.QUERY_TRACE) {
								StringBuilder log = new StringBuilder(256);
								log.append("NOT TESTING CONSTRAINT ").append(owningModuleName).append('.').append(documentName).append('.').append(constraint.getName());
								log.append(" as field ").append(fieldName).append(" is null");
								QUERY_LOGGER.info(log.toString());
							}
							nullParameter = true;
							break; // stop checking the field names of this constraint
						}
						uniqueKey.append(constraintFieldValue.toString()).append('|');
					}
					if (nullParameter) {
						continue; // iterate to next constraint
					}

					String hash = DigestUtils.sha256Hex(uniqueKey.toString());
					if (! uniqueHashes.add(hash)) { // this transaction has this hash already
						throwUniqueConstraintViolationException(constraint, document, bean);
					}
					else {
						try {
							final Persistent persistent = new Persistent();
							persistent.setName(UniquenessEntity.TABLE_NAME);
							String persistentIdentifier = persistent.getPersistentIdentifier();
							StringBuilder sql = new StringBuilder(64);
							sql.append("insert into ").append(persistentIdentifier).append(" (").append(UniquenessEntity.HASH_COLUMN_NAME);
							sql.append(") values (:").append(UniquenessEntity.HASH_COLUMN_NAME).append(')');
							newSQL(sql.toString()).putParameter(UniquenessEntity.HASH_COLUMN_NAME, hash, false).execute();
						}
						catch (@SuppressWarnings("unused") DomainException e) {
							// Unique constraint violation caught here - within the same transaction
							throwUniqueConstraintViolationException(constraint, document, bean);
						}
					}
				}
				
				StringBuilder queryString = new StringBuilder(48);
				queryString.append("select bean from ").append(entityName).append(" as bean");
				
				setFilters(document, scope.toDocumentPermissionScope());

				// indicates if we have appended any where clause conditions
				boolean noWhere = true;

				// Don't check unique constraints if any of the parameters is an unpersisted bean.
				// The query will produce an error and there is no use anyway as there cannot possibly be unique constraint violation.
				boolean unpersistedBeanParameter = false;
				
				// Don't check unique constraints if all of the parameters haven't changed and the bean is persisted
				boolean persistedBeanAndNoDirtyParameters = persisted;
				
				List<Object> constraintFieldValues = new ArrayList<>();
				int i = 1;
				for (String fieldName : constraint.getFieldNames()) {
					Object constraintFieldValue = null;
					try {
						constraintFieldValue = BindUtil.get(bean, fieldName);
					}
					catch (Exception e) {
						throw new DomainException(e);
					}
					
					// Don't do the constraint check if any query parameter is null
					if (constraintFieldValue == null) {
						if (UtilImpl.QUERY_TRACE) {
							StringBuilder log = new StringBuilder(256);
							log.append("NOT TESTING CONSTRAINT ").append(owningModuleName).append('.').append(documentName).append('.').append(constraint.getName());
							log.append(" as field ").append(fieldName).append(" is null");
							QUERY_LOGGER.info(log.toString());
						}
						nullParameter = true;
						break; // stop checking the field names of this constraint
					}
					
					// Don't do the constraint check if any query parameters is not persisted
					if ((constraintFieldValue instanceof PersistentBean) && (! isPersisted((Bean) constraintFieldValue))) {
						if (UtilImpl.QUERY_TRACE) {
							StringBuilder log = new StringBuilder(256);
							log.append("NOT TESTING CONSTRAINT ").append(owningModuleName).append('.').append(documentName).append('.').append(constraint.getName());
							log.append(" as field ").append(fieldName).append(" with value ").append(constraintFieldValue).append(" is not persisted");
							QUERY_LOGGER.info(log.toString());
						}
						unpersistedBeanParameter = true;
						break; // stop checking the field names of this constraint
					}

					if (persistedBeanAndNoDirtyParameters) {
						// Check if the query parameter is dirty
						TargetMetaData target = BindUtil.getMetaDataForBinding(customer, owningModule, document, fieldName);
						Attribute attribute = target.getAttribute();
						// Implicit attribute, so we have to assume we need to do the check
						if (attribute == null) { // implicit
							if (UtilImpl.QUERY_TRACE) {
								StringBuilder log = new StringBuilder(256);
								log.append("TEST CONSTRAINT ").append(owningModuleName).append('.').append(documentName).append('.').append(constraint.getName());
								log.append(" as field ").append(fieldName).append(" is an implicit attribute");
								QUERY_LOGGER.info(log.toString());
							}
							persistedBeanAndNoDirtyParameters = false;
						}
						else {
							// Track changes is on so we can check to see if its dirty or not.
							if (attribute.isTrackChanges()) {
								if (bean.originalValues().containsKey(fieldName)) {
									if (UtilImpl.QUERY_TRACE) {
										StringBuilder log = new StringBuilder(256);
										log.append("TEST CONSTRAINT ").append(owningModuleName).append('.').append(documentName).append('.').append(constraint.getName());
										log.append(" as field ").append(fieldName).append(" has changed");
										QUERY_LOGGER.info(log.toString());
									}
									persistedBeanAndNoDirtyParameters = false;
								}
							}
							// Track changes is off, so we have to assume we need to do the check
							else {
								if (UtilImpl.QUERY_TRACE) {
									StringBuilder log = new StringBuilder(256);
									log.append("TEST CONSTRAINT ").append(owningModuleName).append('.').append(documentName).append('.').append(constraint.getName());
									log.append(" as field ").append(fieldName).append(" has track changes off");
									QUERY_LOGGER.info(log.toString());
								}
								persistedBeanAndNoDirtyParameters = false;
							}
						}
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
					queryString.append(" = ?").append(i++);
				}
	
				if (nullParameter || unpersistedBeanParameter || persistedBeanAndNoDirtyParameters) {
					continue; // iterate to next constraint
				}

				Query<?> query = session.createQuery(queryString.toString());
				if (UtilImpl.QUERY_TRACE) {
					StringBuilder log = new StringBuilder(256);
					log.append("TEST CONSTRAINT ").append(owningModuleName).append('.').append(documentName).append('.').append(constraint.getName());
					log.append(" using ").append(queryString);
					QUERY_LOGGER.info(log.toString());
				}
				query.setLockMode("bean", LockMode.READ); // take a read lock on all referenced documents
				
				// Set timeout if applicable
				int timeout = UtilImpl.DATA_STORE.getOltpConnectionTimeoutInSeconds();
				if (timeout > 0) {
					query.setTimeout(timeout);
				}

				int index = 1;
				for (@SuppressWarnings("unused") String fieldName : constraint.getFieldNames()) {
					Object value = constraintFieldValues.get(index - 1);
					query.setParameter(index, value);
					if (UtilImpl.QUERY_TRACE) {
					    QUERY_LOGGER.info("    SET PARAM " + index + " = " + value);
					}
					index++;
				}
	
				// Use a scrollable result set in case the result set is massive
				try (ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY)) {
					if (results.next()) {
						boolean persistent = isPersisted(bean);
						Bean first = (Bean) results.get()[0];
						if ((! persistent) || // we are inserting and 1 already exists
								results.next() || // more than 1 exists
								(persistent && (! first.getBizId().equals(bean.getBizId())))) { // updating, and 1 exists that is not this ID
							throwUniqueConstraintViolationException(constraint, document, bean);
						}
					}
				}
			}
		}
		finally {
			resetFilters(document);
		}
	}
	
	private static void throwUniqueConstraintViolationException(@Nonnull UniqueConstraint constraint,
																	@Nonnull Document document,
																	@Nonnull Bean bean) {
		String message = null;
		try {
			message = BindUtil.formatMessage(constraint.getMessage(), bean);
		}
		catch (Exception e) {
			e.printStackTrace();
			message = "Unique Constraint Violation occurred but could not display the unique constraint message for constraint " +
							constraint.getName();
		}

		throw new UniqueConstraintViolationException(document, constraint.getName(), message);
	}

	/**
	 * This is a stack so that delete operations called in preDelete bizlet methods will work correctly.
	 * 
	 * The beanToDelete is put into the Map under the key of "" with a singleton set.
	 * The beanToDelete is used to detect the same event in preRemove.
	 * 
	 * Other beans that will be cascaded are put using 
	 * module.document -> set of beans that are being deleted during the delete operation.
	 * This attribute holds all beans being deleted during the delete operation, by module.document,
	 * so that when we check referential integrity, we can ensure that we do not include links to these beans
	 * which will be deleted (by cascading) anyway.
	 * 
	 * This stack is popped and the set variable is cleared at the end of the delete operation.
	 * See the finally block below.
	 * The beans to delete are collected by delete() firstly.
	 * The referential integrity test is done in the preDelete() callback.
	 */
	private Stack<Map<String, Set<Bean>>> deleteContext = new Stack<>();

	/**
	 * Delete a document bean from the data store.
	 */
	@Override
	public final <T extends PersistentBean> void delete(Document document, T bean) {
		if (isPersisted(bean)) {
			try {
				CustomerImpl internalCustomer = (CustomerImpl) getUser().getCustomer();
				boolean vetoed = internalCustomer.interceptBeforeDelete(document, bean);
				if (! vetoed) {
					Set<PersistentBean> staticBeansToDelete = new TreeSet<>();
					boolean dynamic = document.isDynamic();

					if (! dynamic) {
						String entityName = getDocumentEntityName(document.getOwningModuleName(), document.getName());
						PersistentBean beanToDelete = (PersistentBean) session.merge(entityName, bean);
						em.flush();
						UtilImpl.populateFully(beanToDelete);
						
						staticBeansToDelete.add(beanToDelete);
					}
					
					// Find any other static beans referenced by dynamic relations
					new BeanVisitor(true, false) {
						@Override
						protected boolean accept(String binding,
													Document visitedDocument,
													Document owningDocument,
													Relation owningRelation,
													Bean visitedBean) throws Exception {
							if ((owningRelation == null) || (owningDocument == null)) { // top level bean or parent
								return true;
							}
							
							if (visitedBean.isPersisted()) {
								if (owningRelation instanceof Reference) {
									Reference reference = (Reference) owningRelation;
									ReferenceType type = reference.getType();
									// Requires cascading
									if (! (AssociationType.aggregation.equals(type) || CollectionType.aggregation.equals(type))) {
										if ((visitedBean != bean) && // not a reference to the top level bean
												(! visitedDocument.isDynamic()) && // visitedBean is not dynamic
												owningRelation.isPersistent() && // persistent relation
												owningDocument.isDynamic()) { // dynamic relation
											staticBeansToDelete.add((PersistentBean) visitedBean);
										}
										return true;
									}
								}
							}

							return false;
						}
					}.visit(document, bean, internalCustomer);
					
					deleteStatic(staticBeansToDelete);
						
					if (document.isDynamic() || document.hasDynamic()) { // document with dynamism somewhere
						dynamicPersistence.delete(bean);
					}

					internalCustomer.interceptAfterDelete(document, bean);
				}
			}
			catch (Throwable t) {
				treatPersistenceThrowable(t, OperationType.update, bean);
			}
		}
	}
	
	private void deleteStatic(@Nonnull Set<PersistentBean> beans) throws Exception {
		Map<String, Set<Bean>> beansToDelete = null;
		for (PersistentBean bean : beans) {
			try {
				// need to merge before validation to ensure that the FK constraints
				// can check for members of collections etc - need the persistent version for this
					
				// Push a new delete context on
				beansToDelete = new TreeMap<>();
				beansToDelete.put("", Collections.singleton(bean));
				deleteContext.push(beansToDelete);
					
				// Call Bizlet preDelete()
				CustomerImpl internalCustomer = (CustomerImpl) getUser().getCustomer();
				Module m = internalCustomer.getModule(bean.getBizModule());
				Document document = m.getDocument(internalCustomer, bean.getBizDocument());
				boolean vetoed = internalCustomer.interceptBeforePreDelete(bean);
				if (! vetoed) {
					Bizlet<Bean> bizlet = document.getBizlet(internalCustomer);
					if (bizlet != null) {
						if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering " + bizlet.getClass().getName() + ".preDelete: " + bean);
						bizlet.preDelete(bean);
						if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting " + bizlet.getClass().getName() + ".preDelete");
					}
					internalCustomer.interceptAfterPreDelete(bean);
				}
					
				Set<String> documentsVisited = new TreeSet<>();
				// Check composed collections/associations here in case we are 
				// deleting a composed collection element or an association directly using p.delete().
				checkReferentialIntegrityOnDelete(document,
													bean,
													documentsVisited,
													beansToDelete,
													false);
				
				String entityName = getDocumentEntityName(document.getOwningModuleName(), document.getName());
				session.delete(entityName, bean);
				em.flush();
			
				// Call Bizlet postDelete()
				vetoed = internalCustomer.interceptBeforePostDelete(bean);
				if (! vetoed) {
					Bizlet<Bean> bizlet = document.getBizlet(internalCustomer);
					if (bizlet != null) {
						if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering " + bizlet.getClass().getName() + ".postDelete: " + bean);
						bizlet.postDelete(bean);
						if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting " + bizlet.getClass().getName() + ".postDelete");
					}
					internalCustomer.interceptAfterPostDelete(bean);
				}
			}
			finally {
				if (beansToDelete != null) { // delete context was pushed
					beansToDelete.clear();
					deleteContext.pop();
				}
			}
		}
	}

	// Do not increase visibility of this method as we don't want it to be public.
	private void checkReferentialIntegrityOnDelete(@Nonnull Document document, 
													@Nonnull PersistentBean bean, 
													@Nonnull Set<String> documentsVisited,
													@Nonnull Map<String, Set<Bean>> beansToBeCascaded,
													boolean preRemove) {
		Customer customer = user.getCustomer();
		List<ExportedReference> refs = ((CustomerImpl) customer).getExportedReferences(document);
		if (refs != null) {
			for (ExportedReference ref : refs) {
				ReferenceType type = ref.getType();
				// Need to check aggregation FKs
				// Need to check collection joining table element_id FKs
				// but do NOT need to check child collection parent_ids as they point back
				// and do NOT need to check embedded associations as they have no *_id FKs
				if (! (CollectionType.child.equals(type) || AssociationType.embedded.equals(type))) {
					// Check composed collections if we are deleting a composed collection element
					// directly using p.delete(), otherwise,
					// if preRemove() is being fired, we should NOT check composed collections or associations
					// as they are going to be deleted by hibernate 
					// as a collection.remove() was performed or an association was nulled.
					if ((! preRemove) ||
							(preRemove && 
								(! CollectionType.composition.equals(type)) && 
								(! AssociationType.composition.equals(type)))) {
						String moduleName = ref.getModuleName();
						String documentName = ref.getDocumentName();
						String modoc = new StringBuilder(64).append(moduleName).append('.').append(documentName).toString();
						Module referenceModule = customer.getModule(moduleName);
						Document referenceDocument = referenceModule.getDocument(customer, documentName);
						Persistent persistent = document.getPersistent();
						if (persistent != null) {
							if (ExtensionStrategy.mapped.equals(persistent.getStrategy())) {
								checkMappedReference(bean, beansToBeCascaded, document, ref, modoc, referenceDocument);
							}
							else {
								checkTypedReference(bean, beansToBeCascaded, document, ref, modoc, referenceDocument);
							}
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
			checkReferentialIntegrityOnDelete(baseDocument, bean, documentsVisited, beansToBeCascaded, preRemove);
		}

		// Process derived documents if present
		for (String derivedDocumentName : ((CustomerImpl) customer).getDerivedDocuments(document)) {
			if ((derivedDocumentName != null) && (! documentsVisited.contains(derivedDocumentName))) {
				int dotIndex = derivedDocumentName.indexOf('.');
				Module derivedModule = customer.getModule(derivedDocumentName.substring(0, dotIndex));
				Document derivedDocument = derivedModule.getDocument(customer, derivedDocumentName.substring(dotIndex + 1));
				checkReferentialIntegrityOnDelete(derivedDocument, bean, documentsVisited, beansToBeCascaded, preRemove);
			}
		}
	}

	private void checkTypedReference(@Nonnull PersistentBean beanToDelete, 
										@Nonnull Map<String, Set<Bean>> beansToBeCascaded,
										@Nonnull Document documentToDelete,
										@Nonnull ExportedReference ref,
										@Nonnull String modoc,
										@Nonnull Document referenceDocument)
	throws ReferentialConstraintViolationException {
		Persistent persistent = referenceDocument.getPersistent();
		if ((persistent != null) && ExtensionStrategy.mapped.equals(persistent.getStrategy())) {
			// Find all implementations below the mapped and check these instead
			Set<Document> derivations = new HashSet<>();
			populateImmediateMapImplementingDerivations((CustomerImpl) user.getCustomer(), referenceDocument, derivations);
			for (Document derivation : derivations) {
				checkTypedReference(beanToDelete, beansToBeCascaded, documentToDelete, ref, modoc, derivation);
			}
		}
		else {
			Set<Bean> beansToBeExcluded = beansToBeCascaded.get(modoc);
			
			// Check static references only - dynamic ones are checked in DynamicPersistence implementation
			if (! (referenceDocument.isDynamic() || documentToDelete.isDynamic())) {
				if (! hasReferentialIntegrity(beanToDelete, ref, referenceDocument, beansToBeExcluded)) {
					throw new ReferentialConstraintViolationException(documentToDelete.getLocalisedSingularAlias(), beanToDelete.getBizKey(), ref.getLocalisedDocumentAlias());
				}
			}
		}
	}
	
	private boolean hasReferentialIntegrity(@Nonnull Bean beanToDelete,
												@Nonnull ExportedReference exportedReference,
												@Nonnull Document referenceDocument,
												@Nullable Set<Bean> beansToBeExcluded) {
		setFilters(referenceDocument, DocumentPermissionScope.global);
		try {
			StringBuilder queryString = new StringBuilder(64);
			queryString.append("select bean from ");
			queryString.append(getDocumentEntityName(referenceDocument.getOwningModuleName(), referenceDocument.getName()));
			queryString.append(" as bean");
			if (exportedReference.isCollection()) {
				// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
				queryString.append(" where :referencedBeanId member of bean.");
				queryString.append(exportedReference.getReferenceFieldName());
			}
			else {
				queryString.append(" where bean.");
				queryString.append(exportedReference.getReferenceFieldName());
				// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
				queryString.append(".bizId = :referencedBeanId");
			}
			
			if (beansToBeExcluded != null) {
				int i = 0;
				for (@SuppressWarnings("unused") Bean thisBeanToBeCascaded : beansToBeExcluded) {
					// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
					queryString.append(" and bean.bizId != :deletedBeanId").append(i++);
				}
			}
			if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("FK check : " + queryString);

			Query<?> query = session.createQuery(queryString.toString());
			query.setLockMode("bean", LockMode.READ); // read lock required for referential integrity

			// Set timeout if applicable
			int timeout = UtilImpl.DATA_STORE.getOltpConnectionTimeoutInSeconds();
			if (timeout > 0) {
				query.setTimeout(timeout);
			}

			// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
			query.setParameter("referencedBeanId", beanToDelete.getBizId(), StringType.INSTANCE);
			if (beansToBeExcluded != null) {
				int i = 0;
				for (Bean thisBeanToBeCascaded : beansToBeExcluded) {
					// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
					query.setParameter("deletedBeanId" + i++, thisBeanToBeCascaded.getBizId(), StringType.INSTANCE);
				}
			}

			try (ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY)) {
				return (! results.next());
			}
		}
		finally {
			resetFilters(referenceDocument);
		}
	}
	
	private void checkMappedReference(@Nonnull PersistentBean bean, 
										@Nonnull Map<String, Set<Bean>> beansToBeCascaded,
										@Nonnull Document document,
										@Nonnull ExportedReference ref,
										@Nonnull String modoc,
										@Nonnull Document referenceDocument) {
		Persistent persistent = referenceDocument.getPersistent();
		if (persistent != null) {
			if (ExtensionStrategy.mapped.equals(persistent.getStrategy())) {
				// Find all implementations below the mapped and check these instead
				Set<Document> derivations = new HashSet<>();
				populateImmediateMapImplementingDerivations((CustomerImpl) user.getCustomer(), referenceDocument, derivations);
				for (Document derivation : derivations) {
					checkMappedReference(bean, beansToBeCascaded, document, ref, modoc, derivation);
				}
			}
			else {
				StringBuilder queryString = new StringBuilder(64);
				queryString.append("select 1 from ");
				queryString.append(persistent.getPersistentIdentifier());
				if (ref.isCollection()) {
					queryString.append('_').append(ref.getReferenceFieldName());
					queryString.append(" where ").append(PersistentBean.ELEMENT_COLUMN_NAME).append(" = :reference_id");
				}
				else {
					queryString.append(" where ").append(ref.getReferenceFieldName());
					queryString.append("_id = :reference_id");
				}
				
				Set<Bean> theseBeansToBeCascaded = beansToBeCascaded.get(modoc);
				if (theseBeansToBeCascaded != null) {
					int i = 0;
					for (@SuppressWarnings("unused") Bean thisBeanToBeCascaded : theseBeansToBeCascaded) {
						if (ref.isCollection()) {
							queryString.append(" and ").append(PersistentBean.OWNER_COLUMN_NAME).append(" != :deleted_id");
						}
						else {
							queryString.append(" and ").append(Bean.DOCUMENT_ID).append(" != :deleted_id");
						}
						queryString.append(i++);
					}
				}
				if (UtilImpl.QUERY_TRACE) QUERY_LOGGER.info("FK check : " + queryString);
		
				NativeQuery<?> query = session.createNativeQuery(queryString.toString());
//				query.setLockMode("bean", LockMode.READ); // read lock required for referential integrity
	
				// Set timeout if applicable
				int timeout = UtilImpl.DATA_STORE.getOltpConnectionTimeoutInSeconds();
				if (timeout > 0) {
					query.setTimeout(timeout);
				}
	
				if (UtilImpl.QUERY_TRACE) {
				    QUERY_LOGGER.info("    SET PARAM reference_id = " + bean.getBizId());
				}
				query.setParameter("reference_id", bean.getBizId(), StringType.INSTANCE);
				if (theseBeansToBeCascaded != null) {
					int i = 0;
					for (Bean thisBeanToBeCascaded : theseBeansToBeCascaded) {
						if (UtilImpl.QUERY_TRACE) {
						    QUERY_LOGGER.info("    SET PARAM deleted_id " + i + " = " + thisBeanToBeCascaded.getBizId());
						}
						query.setParameter("deleted_id" + i++, thisBeanToBeCascaded.getBizId(), StringType.INSTANCE);
					}
				}
		
				try (ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY)) {
					if (results.next()) {
						throw new ReferentialConstraintViolationException(document.getLocalisedSingularAlias(), bean.getBizKey(), ref.getLocalisedDocumentAlias());
					}
				}
			}
		}
	}

	private void populateImmediateMapImplementingDerivations(@Nonnull CustomerImpl customer,
																@Nonnull Document document,
																@Nonnull Set<Document> result) {
		for (String derivedDocumentName : customer.getDerivedDocuments(document)) {
			int dotIndex = derivedDocumentName.indexOf('.');
			Module derivedModule = customer.getModule(derivedDocumentName.substring(0, dotIndex));
			Document derivedDocument = derivedModule.getDocument(customer, derivedDocumentName.substring(dotIndex + 1));

			if (derivedDocument.isPersistable()) {
				result.add(derivedDocument);
			}
			else {
				populateImmediateMapImplementingDerivations(customer, derivedDocument, result);
			}
		}
	}
	
	@Override
	public <T extends Bean> T retrieve(Document document, String id) {
		return retrieve(document, id, false);
	}
	
	@Override
	public <T extends Bean> T retrieveAndLock(Document document, String id) {
		T result = retrieve(document, id, true);
		if (result == null) {
			throw new NoResultsException();
		}
		return result;
	}
	
	@SuppressWarnings("unchecked")
	private final @Nullable <T extends Bean> T retrieve(@Nonnull Document document, @Nonnull String id, boolean forUpdate) {
		T result = null;
		Class<?> beanClass = null;
		String entityName = getDocumentEntityName(document.getOwningModuleName(), document.getName());
		Customer customer = user.getCustomer();
		try {
			if (UtilImpl.USING_JPA && (! entityName.startsWith(customer.getName()))) {
				beanClass = ((DocumentImpl) document).getBeanClass(customer);
			}
			if (document.isDynamic()) {
				try {
					result = (T) dynamicPersistence.populate(id);
				}
				catch (Exception e) {
					// throw this to be caught below
					throw new ClassNotFoundException("Could not create a new instance of document " + document.getOwningModuleName() + "." + document.getName(), e);
				}
			}
			else {
				if (forUpdate) {
					if (beanClass != null) {
						result = (T) session.load(beanClass, id, LockMode.PESSIMISTIC_WRITE);
					}
					else {
						result = (T) session.load(entityName, id, LockMode.PESSIMISTIC_WRITE);
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
		}
		catch (@SuppressWarnings("unused") StaleObjectStateException e) { // thrown from session.load() with LockMode.UPGRADE
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

			session.refresh(result, LockMode.PESSIMISTIC_WRITE);
		}
		catch (ClassNotFoundException e) { // Can emanate out of hibernate innards
			throw new MetaDataException("Could not find bean", e);
		}

		return result;
	}

	@Override
	public void postLoad(PersistentBean loadedBean)
	throws Exception {
		// Inject any dependencies
		UtilImpl.inject(loadedBean);

		Customer customer = user.getCustomer();
		Module module = customer.getModule(loadedBean.getBizModule());
		Document document = module.getDocument(customer, loadedBean.getBizDocument());

		((DocumentImpl) document).populateDynamicAttributeDefaults(customer, loadedBean);

		if (document.hasDynamic()) {
			dynamicPersistence.populate(loadedBean);
		}

		// check that embedded objects are empty and null them if they are
		nullEmbeddedReferencesOnLoad(customer, module, document, loadedBean);
		
		CustomerImpl internalCustomer = (CustomerImpl) customer;
		boolean vetoed = internalCustomer.interceptBeforePostLoad(loadedBean);
		if (! vetoed) {
			Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
			if (bizlet != null) {
				if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering " + bizlet.getClass().getName() + ".postLoad: " + loadedBean);
				bizlet.postLoad(loadedBean);
				if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting " + bizlet.getClass().getName() + ".postLoad");
			}
			internalCustomer.interceptAfterPostLoad(loadedBean);
		}

		// clear the object's dirtiness
		loadedBean.originalValues().clear();
	}

	private static void nullEmbeddedReferencesOnLoad(@Nonnull Customer customer,
														@Nonnull Module module,
														@Nonnull Document document,
														@Nonnull PersistentBean loadedBean) {
		for (Attribute attribute : document.getAllAttributes(customer)) {
			if (attribute instanceof Association) {
				Association association = (Association) attribute;
				if (AssociationType.embedded.equals(association.getType())) {
					String embeddedName = association.getName();
					Bean embeddedBean = (Bean) BindUtil.get(loadedBean, embeddedName);
					if (embeddedBean != null) {
						Document embeddedDocument = module.getDocument(customer, association.getDocumentName());
						boolean empty = true;
						for (Attribute embeddedAttribute : embeddedDocument.getAllAttributes(customer)) {
							// ignore inverses since they are stored directly in the data store
							if (! (embeddedAttribute instanceof Inverse)) {
								Object value = BindUtil.get(embeddedBean, embeddedAttribute.getName());
								if (value != null) {
									if ((value instanceof List<?>) && ((List<?>) value).isEmpty()) {
										continue;
									}
									empty = false;
									break;
								}
							}
						}
						if (empty) {
							BindUtil.set(loadedBean, embeddedName, null);
							// clear the object's dirtiness read for interceptor and bizlet callbacks
							loadedBean.originalValues().remove(embeddedName);
						}
					}
				}
			}
		}
	}
	
	@Override
	public void reindex(PersistentBean beanToReindex)
	throws Exception {
		TextExtractor extractor = null; // lazily instantiated
		BeanContent content = new BeanContent(beanToReindex);
		Map<String, String> properties = content.getProperties();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(beanToReindex.getBizModule());
		Document document = module.getDocument(customer, beanToReindex.getBizDocument());
		for (Attribute attribute : document.getAllAttributes(customer)) {
			if (attribute instanceof Field) {
				Field field = (Field) attribute;
				AttributeType type = attribute.getAttributeType();
				IndexType index = field.getIndex();
				if (IndexType.textual.equals(index) || IndexType.both.equals(index)) {
					String fieldName = field.getName();
					String value = BindUtil.getDisplay(customer, beanToReindex, fieldName);
					if (AttributeType.markup.equals(type)) {
						if (extractor == null) {
							extractor = EXT.getAddInManager().getExtension(TextExtractor.class);
						}
						if (extractor != null) {
							value = extractor.extractTextFromMarkup(value);
						}
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

	public void index(@Nonnull PersistentBean beanToIndex,
						@Nonnull String[] propertyNames,
						@Nonnull Type[] propertyTypes,
						@Nullable Object[] oldState,
						@Nonnull Object[] state)
	throws Exception {
		BeanContent content = new BeanContent(beanToIndex);
		Map<String, String> properties = content.getProperties();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(beanToIndex.getBizModule());
		Document document = module.getDocument(customer, beanToIndex.getBizDocument());
		TextExtractor extractor = null; // lazily instantiated

		for (int i = 0, l = propertyNames.length; i < l; i++) {
			String propertyName = propertyNames[i];

			// NB use getMetaForBinding() to ensure that base document attributes are also retrieved
			TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, propertyName);
			Attribute attribute = target.getAttribute();
			if (attribute instanceof Field) {
				Field field = (Field) attribute;
				AttributeType type = field.getAttributeType();
				IndexType index = field.getIndex();
				if (IndexType.textual.equals(index) || IndexType.both.equals(index)) {
					if (oldState != null) { // an update
						if (! propertyTypes[i].isEqual(state[i], oldState[i])) {
							String value = (state[i] == null) ? null : state[i].toString();
							if (value != null) {
								if (AttributeType.markup.equals(type)) {
									if (extractor == null) {
										extractor = EXT.getAddInManager().getExtension(TextExtractor.class);
									}
									if (extractor != null) {
										value = extractor.extractTextFromMarkup(value);
									}
								}
								if (value != null) {
									properties.put(propertyName, value);
								}
							}
						}
					}
					else {
						String value = (state[i] == null) ? null : state[i].toString();
						if (value != null) {
							if (AttributeType.markup.equals(type)) {
								if (extractor == null) {
									extractor = EXT.getAddInManager().getExtension(TextExtractor.class);
								}
								if (extractor != null) {
									value = extractor.extractTextFromMarkup(value);
								}
							}
							if (value != null) {
								properties.put(propertyName, value);
							}
						}
					}
				}
				// Do not remove attachment content here...
				// If the transaction rolls back and we are using a file store content manager
				// the file could be deleted when its not meant to be orphaning the content link.
				// Let Content Garbage Collection pick it up in another thread once committed to the RDBMS.
				/*
				if (AttributeType.content.equals(type) || AttributeType.image.equals(type)) {
					if (oldState != null) { // an update
						if ((state[i] == null) && (oldState[i] != null)) { // removed the content link
							// Remove the attachment content
							removeAttachmentContent((String) oldState[i]);
						}
					}
				}
				*/
			}
		}

		if (! properties.isEmpty()) {
			putBeanContent(content);
		}
	}

	// Need the callback because an element removed from a collection will be deleted and only this event will pick it up
	@Override
	public void preRemove(PersistentBean bean)
	throws Exception {
		final Map<String, Set<Bean>> beansToDelete = deleteContext.isEmpty() ? new TreeMap<>() : deleteContext.peek();

		if (! deleteContext.isEmpty()) { // called within a Persistence.delete() operation 
			// Don't continue if we've already called preDelete on this bean 
			// as it was the argument in a Persistence.delete() call
			Bean beanToDelete = beansToDelete.get("").stream().findFirst().get();
			if (bean.equals(beanToDelete)) {
				return;
			}
		}
		
		final Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		
		// Collect beans to be cascaded
		new CascadeDeleteBeanVisitor() {
			@Override
			public void preDeleteProcessing(Document documentToCascade, Bean beanToCascade) 
			throws Exception {
				add(documentToCascade, beanToCascade);
			}
			
			private void add(Document documentToCascade, Bean beanToCascade) 
			throws Exception {
				String modoc = new StringBuilder(64).append(documentToCascade.getOwningModuleName()).append('.').append(documentToCascade.getName()).toString();
				Set<Bean> theseBeansToDelete = beansToDelete.get(modoc);
				if (theseBeansToDelete == null) {
					theseBeansToDelete = new TreeSet<>();
					beansToDelete.put(modoc, theseBeansToDelete);
				}
				theseBeansToDelete.add(beanToCascade);

				// Ensure that this bean is registered against any module.document defined in its base documents too
				Extends inherits = documentToCascade.getExtends();
				if (inherits != null) {
					Document baseDocument = customer.getModule(documentToCascade.getOwningModuleName()).getDocument(customer, inherits.getDocumentName());
					add(baseDocument, beanToCascade);
				}
			}
		}.visit(document, bean, customer);
		
		try {
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePreDelete(bean);
			if (! vetoed) {
				Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering " + bizlet.getClass().getName() + ".preDelete: " + bean);
					bizlet.preDelete(bean);
					if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting " + bizlet.getClass().getName() + ".preDelete");
				}
				internalCustomer.interceptAfterPreDelete(bean);
			}

			Set<String> documentsVisited = new TreeSet<>();
			// We should NOT check composed collections/associations here 
			// as they are going to be deleted by hibernate 
			// as a collection.remove() was performed or an association was nulled.
			checkReferentialIntegrityOnDelete(document,
												bean,
												documentsVisited,
												beansToDelete,
												true);
			bean.setBizLock(new OptimisticLock(user.getName(), new Date()));
		}
		catch (ValidationException e) {
			for (Message message : e.getMessages()) {
				ValidationUtil.processMessageBindings(customer, message, bean, bean);
			}
			throw e;
		}
	}

	@Override
	public void postRemove(PersistentBean bean)
	throws Exception {
		// check we are not calling postRemove from delete operation
		final Map<String, Set<Bean>> beansToDelete = deleteContext.isEmpty() ? new TreeMap<>() : deleteContext.peek();
		if (! deleteContext.isEmpty()) { // called within a Persistence.delete() operation 
			// Don't continue if we've already called preDelete on this bean 
			// as it was the argument in a Persistence.delete() call
			Bean beanToDelete = beansToDelete.get("").stream().findFirst().get();
			if (bean.equals(beanToDelete)) {
				// remove content but don't call Bizlet.postDelete()
				removeBeanContent(bean);
				return;
			}
		}

		// call Bizlet.postDelete() and then remove content
		final Customer customer = user.getCustomer();
		try {
			final CustomerImpl internalCustomer = (CustomerImpl) customer;
			Module module = internalCustomer.getModule(bean.getBizModule());
			Document document = module.getDocument(customer, bean.getBizDocument());
			boolean vetoed = internalCustomer.interceptBeforePreDelete(bean);
			if (! vetoed) {
				Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering " + bizlet.getClass().getName() + ".postDelete: " + bean);
					bizlet.postDelete(bean);
					if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting " + bizlet.getClass().getName() + ".postDelete");
				}
				internalCustomer.interceptAfterPreDelete(bean);
			}
		}
		catch (ValidationException e) {
			for (Message message : e.getMessages()) {
				ValidationUtil.processMessageBindings(customer, message, bean, bean);
			}
			throw e;
		}

		// remove content
		removeBeanContent(bean);
	}
	
	public final @Nonnull Connection getConnection() {
/*
Maybe use this...
public void doWorkOnConnection(Session session) {
  session.doWork(new Work() {
    public void execute(Connection connection) throws SQLException {
      //use the connection here...
    }
  });
}
*/
		return ((SessionImpl) session).connection();
	}
	
	private static final Integer NEW_VERSION = Integer.valueOf(0);
	
	@Override
	public void upsertBeanTuple(PersistentBean bean) {
		CustomerImpl customer = (CustomerImpl) user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		if (! document.isPersistable()) {
			throw new MetaDataException("Document " + module.getName() + '.' + document.getName() + " is not persistable");
		}
		@SuppressWarnings("null") // tested above
		String persistentIdentifier = document.getPersistent().getPersistentIdentifier();
		
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
			query.append("update ").append(persistentIdentifier).append(" set ");
			query.append(PersistentBean.VERSION_NAME).append('=').append(PersistentBean.VERSION_NAME).append("+1");
			query.append(',').append(PersistentBean.LOCK_NAME).append("=:").append(PersistentBean.LOCK_NAME);
			query.append(',').append(PersistentBean.FLAG_COMMENT_NAME).append("=:").append(PersistentBean.FLAG_COMMENT_NAME);
			query.append(',').append(Bean.CUSTOMER_NAME).append("=:").append(Bean.CUSTOMER_NAME);
			query.append(',').append(Bean.DATA_GROUP_ID).append("=:").append(Bean.DATA_GROUP_ID);
			query.append(',').append(Bean.USER_ID).append("=:").append(Bean.USER_ID);
			query.append(',').append(Bean.BIZ_KEY).append("=:").append(Bean.BIZ_KEY);
			if (parentDocumentName != null) {
				if (parentDocumentName.equals(document.getName())) {
					query.append(',').append(HierarchicalBean.PARENT_ID).append("=:").append(HierarchicalBean.PARENT_ID);
				}
				else {
					query.append(',').append(ChildBean.CHILD_PARENT_ID).append("=:").append(ChildBean.CHILD_PARENT_ID);
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
					Association association = (Association) attribute;
					// Exclude embedded associations
					if (association.getType() != AssociationType.embedded) {
						query.append(',').append(attributeName).append("_id=:").append(attributeName).append("_id");

						// If this is an arc, add the type column to the insert
						String referencedDocumentName = association.getDocumentName();
						Document referencedDocument = module.getDocument(customer, referencedDocumentName);
						Persistent referencedPersistent = referencedDocument.getPersistent();
						if ((referencedPersistent != null) && ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy())) {
							query.append(',').append(attributeName).append("_type=:").append(attributeName).append("_type");
						}
					}
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
			columns.append(PersistentBean.LOCK_NAME).append(',').append(Bean.BIZ_KEY).append(',').append(PersistentBean.FLAG_COMMENT_NAME).append(',');
			columns.append(Bean.CUSTOMER_NAME).append(',').append(Bean.DATA_GROUP_ID).append(',').append(Bean.USER_ID);
			StringBuilder values = new StringBuilder(128);
			values.append(':').append(Bean.DOCUMENT_ID).append(",:").append(PersistentBean.VERSION_NAME).append(",:");
			values.append(PersistentBean.LOCK_NAME).append(",:").append(Bean.BIZ_KEY).append(",:").append(PersistentBean.FLAG_COMMENT_NAME).append(",:");
			values.append(Bean.CUSTOMER_NAME).append(",:").append(Bean.DATA_GROUP_ID).append(",:").append(Bean.USER_ID);

			// Add parent if required
			if (parentDocumentName != null) {
				if (parentDocumentName.equals(document.getName())) {
					columns.append(',').append(HierarchicalBean.PARENT_ID);
					values.append(",:").append(HierarchicalBean.PARENT_ID);
				}
				else {
					columns.append(',').append(ChildBean.CHILD_PARENT_ID);
					values.append(",:").append(ChildBean.CHILD_PARENT_ID);
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
					Association association = (Association) attribute;
					// Exclude embedded associations
					if (association.getType() != AssociationType.embedded) {
						columns.append(',').append(attributeName).append("_id");
						values.append(",:").append(attributeName).append("_id");
	
						// If this is an arc, add the type column to the insert
						String referencedDocumentName = association.getDocumentName();
						Document referencedDocument = module.getDocument(customer, referencedDocumentName);
						Persistent referencedPersistent = referencedDocument.getPersistent();
						if ((referencedPersistent != null) && ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy())) {
							columns.append(',').append(attributeName).append("_type");
							values.append(",:").append(attributeName).append("_type");
						}
					}
				}
				else if (attribute instanceof Field) {
					columns.append(',').append(attributeName);
					values.append(",:").append(attributeName);
				}
			}

			// build the query
			query.append(" insert into ").append(persistentIdentifier).append(" (");
			query.append(columns).append(") values (").append(values).append(')');
		}

		// bind the built in parameters
		SQL sql = newSQL(query.toString());
		sql.putParameter(Bean.DOCUMENT_ID, bean.getBizId(), false);
		bean.setBizLock(new OptimisticLock(user.getName(), new Date()));
		sql.putParameter(PersistentBean.LOCK_NAME, bean.getBizLock().toString(), false);
		if (! bean.isPersisted()) {
			sql.putParameter(PersistentBean.VERSION_NAME, NEW_VERSION);
		}
		sql.putParameter(PersistentBean.FLAG_COMMENT_NAME, bean.getBizFlagComment(), true);
		sql.putParameter(Bean.CUSTOMER_NAME, bean.getBizCustomer(), false);
		sql.putParameter(Bean.DATA_GROUP_ID, bean.getBizDataGroupId(), false);
		sql.putParameter(Bean.USER_ID, bean.getBizUserId(), false);
		sql.putParameter(Bean.BIZ_KEY, Util.processStringValue(bean.getBizKey()), false);

		// Bind parent if required
		if (parentDocumentName != null) {
			if (parentDocumentName.equals(document.getName())) {
				sql.putParameter(HierarchicalBean.PARENT_ID, ((HierarchicalBean<?>) bean).getBizParentId(), false);
			}
			else {
				Bean parent = ((ChildBean<?>) bean).getParent();
				sql.putParameter(ChildBean.CHILD_PARENT_ID, (parent == null) ? null : parent.getBizId(), false);
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
					Association association = (Association) attribute;
					// Exclude embedded associations
					if (association.getType() != AssociationType.embedded) {
						String columnName = new StringBuilder(64).append(attributeName).append("_id").toString();
						String binding = new StringBuilder(64).append(attributeName).append('.').append(Bean.DOCUMENT_ID).toString();
						sql.putParameter(columnName, (String) BindUtil.get(bean, binding), false);
	
						// If this is an arc, add the type column to the insert
						String referencedDocumentName = association.getDocumentName();
						Document referencedDocument = module.getDocument(customer, referencedDocumentName);
						Persistent referencedPersistent = referencedDocument.getPersistent();
						if ((referencedPersistent != null) && ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy())) {
							columnName = new StringBuilder(64).append(attributeName).append("_type").toString();
							Bean referencedBean = (Bean) BindUtil.get(bean, attributeName);
							String value = null;
							if (referencedBean != null) {
								value = new StringBuilder(64).append(referencedBean.getBizModule()).append('.').append(referencedBean.getBizDocument()).toString();
							}
							sql.putParameter(columnName, value, false);
						}
					}
				}
				else if (attribute instanceof Enumeration) {
					org.skyve.domain.types.Enumeration value = (org.skyve.domain.types.Enumeration) BindUtil.get(bean, attributeName);
					sql.putParameter(attributeName, value);
				}
				else if (attribute instanceof Field) {
					List<DomainValue> domainValues = null;
					DomainType domainType = attribute.getDomainType();
					if (domainType != null) {
						domainValues = ((DocumentImpl) document).getDomainValues(customer, domainType, attribute, bean, true);
					}
					Object value = BindUtil.get(bean, attributeName);
					if (domainValues != null) {
						for (DomainValue domainValue : domainValues) {
							if (domainValue.getLocalisedDescription().equals(value)) {
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
	public void upsertCollectionTuples(PersistentBean owningBean, String collectionName) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(owningBean.getBizModule());
		Document document = module.getDocument(customer, owningBean.getBizDocument());
		if (! document.isPersistable()) {
			throw new MetaDataException("Document " + module.getName() + '.' + document.getName() + " is not persistable");
		}
		@SuppressWarnings("null") // tested above
		String persistentIdentifier = document.getPersistent().getPersistentIdentifier();
		
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
		
		if (elementBeans != null) {
			for (Bean elementBean : elementBeans) {
				query.append("select * from ").append(persistentIdentifier).append('_').append(collectionName);
				query.append(" where ").append(PersistentBean.OWNER_COLUMN_NAME).append("=:");
				query.append(PersistentBean.OWNER_COLUMN_NAME).append(" and ").append(PersistentBean.ELEMENT_COLUMN_NAME);
				query.append("=:").append(PersistentBean.ELEMENT_COLUMN_NAME);
	
				SQL sql = newSQL(query.toString());
				sql.putParameter(PersistentBean.OWNER_COLUMN_NAME, owningBean.getBizId(), false);
				sql.putParameter(PersistentBean.ELEMENT_COLUMN_NAME, elementBean.getBizId(), false);
	
				boolean notExists = sql.tupleResults().isEmpty();
				query.setLength(0);
				if (notExists) {
					query.append("insert into ").append(persistentIdentifier).append('_').append(collectionName);
					query.append(" (").append(PersistentBean.OWNER_COLUMN_NAME).append(',').append(PersistentBean.ELEMENT_COLUMN_NAME);
					query.append(") values (:").append(PersistentBean.OWNER_COLUMN_NAME).append(",:");
					query.append(PersistentBean.ELEMENT_COLUMN_NAME).append(')');
	
					sql = newSQL(query.toString());
					sql.putParameter(PersistentBean.OWNER_COLUMN_NAME, owningBean.getBizId(), false);
					sql.putParameter(PersistentBean.ELEMENT_COLUMN_NAME, elementBean.getBizId(), false);
	
					sql.execute();
					query.setLength(0);
				}
			}
		}
	}
	
	@Override
	public void insertCollectionTuples(PersistentBean owningBean, String collectionName) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(owningBean.getBizModule());
		Document document = module.getDocument(customer, owningBean.getBizDocument());
		if (! document.isPersistable()) {
			throw new MetaDataException("Document " + module.getName() + '.' + document.getName() + " is not persistable");
		}
		@SuppressWarnings("null") // tested above
		String persistentIdentifier = document.getPersistent().getPersistentIdentifier();
		StringBuilder query = new StringBuilder(256);
		query.append("insert into ").append(persistentIdentifier).append('_').append(collectionName);
		query.append(" (").append(PersistentBean.OWNER_COLUMN_NAME).append(',').append(PersistentBean.ELEMENT_COLUMN_NAME);
		query.append(") values (:").append(PersistentBean.OWNER_COLUMN_NAME).append(",:");
		query.append(PersistentBean.ELEMENT_COLUMN_NAME).append(')');

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
		
		if (elementBeans != null) {
			for (Bean elementBean : elementBeans) {
				SQL sql = newSQL(query.toString());
				sql.putParameter(PersistentBean.OWNER_COLUMN_NAME, owningBean.getBizId(), false);
				sql.putParameter(PersistentBean.ELEMENT_COLUMN_NAME, elementBean.getBizId(), false);
	
				sql.execute();
			}
		}
	}

	/**
	 * In case of emergency, break glass
	 */
	public final @Nonnull EntityManager getEntityManager() {
		return em;
	}
	
	/**
	 * In case of emergency, break glass
	 */
	public final @Nonnull Session getSession() {
		return session;
	}

	@Override
	public SQL newSQL(String query) {
		return new HibernateSQL(query, this);
	}

	@Override
	public SQL newNamedSQL(String moduleName, String queryName) {
		Module module = user.getCustomer().getModule(moduleName);
		SQLDefinition sql = module.getSQL(queryName);
		HibernateSQL result = new HibernateSQL(sql.getQuery(), this);
		result.setTimeoutInSeconds(sql.getTimeoutInSeconds());
		return result;
	}

	@Override
	public SQL newNamedSQL(Module module, String queryName) {
		SQLDefinition sql = module.getSQL(queryName);
		HibernateSQL result = new HibernateSQL(sql.getQuery(), this);
		result.setTimeoutInSeconds(sql.getTimeoutInSeconds());
		return result;
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
	public SQL newNamedSQL(String moduleName, String documentName, String queryName) {
		Module module = user.getCustomer().getModule(moduleName);
		SQLDefinition sql = module.getSQL(queryName);
		HibernateSQL result = new HibernateSQL(moduleName, documentName, sql.getQuery(), this);
		result.setTimeoutInSeconds(sql.getTimeoutInSeconds());
		return result;
	}

	@Override
	public SQL newNamedSQL(Document document, String queryName) {
		Module module = user.getCustomer().getModule(document.getOwningModuleName());
		SQLDefinition sql = module.getSQL(queryName);
		HibernateSQL result = new HibernateSQL(document, sql.getQuery(), this);
		result.setTimeoutInSeconds(sql.getTimeoutInSeconds());
		return result;
	}

	@Override
	public BizQL newBizQL(String query) {
		return new HibernateBizQL(query, this);
	}

	@Override
	public BizQL newNamedBizQL(String moduleName, String queryName) {
		Module module = user.getCustomer().getModule(moduleName);
		BizQLDefinition bizql = module.getBizQL(queryName);
		HibernateBizQL result = new HibernateBizQL(bizql.getQuery(), this);
		result.setTimeoutInSeconds(bizql.getTimeoutInSeconds());
		return result;
	}

	@Override
	public BizQL newNamedBizQL(Module module, String queryName) {
		BizQLDefinition bizql = module.getBizQL(queryName);
		HibernateBizQL result = new HibernateBizQL(bizql.getQuery(), this);
		result.setTimeoutInSeconds(bizql.getTimeoutInSeconds());
		return result;
	}

	@Override
	public DocumentQuery newNamedDocumentQuery(String moduleName, String queryName) {
		Module module = user.getCustomer().getModule(moduleName);
		MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
		return query.constructDocumentQuery(null, null);
	}

	@Override
	public DocumentQuery newNamedDocumentQuery(Module module, String queryName) {
		MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
		return query.constructDocumentQuery(null, null);
	}

	@Override
	public DocumentQuery newDocumentQuery(Document document) {
		return new HibernateDocumentQuery(document, this);
	}

	@Override
	public DocumentQuery newDocumentQuery(String moduleName, String documentName) {
		return new HibernateDocumentQuery(moduleName, documentName, this);
	}

	@Override
	public DocumentQuery newDocumentQuery(Document document, String fromClause, String filterClause, String groupClause, String orderClause) {
		return new HibernateDocumentQuery(document, fromClause, filterClause, groupClause, orderClause, this);
	}

	@Override
	public DocumentQuery newDocumentQuery(Bean queryByExampleBean)
	throws Exception {
		return new HibernateDocumentQuery(queryByExampleBean, this);
	}
}
