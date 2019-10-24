package org.skyve.impl.persistence.hibernate;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.logging.Level;

import javax.persistence.EntityManager;
import javax.persistence.EntityNotFoundException;
import javax.persistence.EntityTransaction;
import javax.persistence.RollbackException;

import org.apache.deltaspike.core.api.provider.BeanProvider;
import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.hibernate.Filter;
import org.hibernate.FlushMode;
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
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.event.service.spi.EventListenerRegistry;
import org.hibernate.event.spi.EventType;
import org.hibernate.integrator.spi.Integrator;
import org.hibernate.integrator.spi.IntegratorService;
import org.hibernate.internal.SessionImpl;
import org.hibernate.query.NativeQuery;
import org.hibernate.query.Query;
import org.hibernate.service.spi.SessionFactoryServiceRegistry;
import org.hibernate.tool.hbm2ddl.SchemaExport;
import org.hibernate.tool.hbm2ddl.SchemaUpdate;
import org.hibernate.tool.schema.TargetType;
import org.hibernate.type.StringType;
import org.hibernate.type.Type;
import org.skyve.content.BeanContent;
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
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.messages.ReferentialConstraintViolationException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.CustomerImpl.ExportedReference;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
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
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.SQL;
import org.skyve.util.BeanVisitor;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;

public abstract class AbstractHibernatePersistence extends AbstractPersistence {
	private static final long serialVersionUID = -1813679859498468849L;

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

	protected abstract void removeBeanContent(PersistentBean bean) throws Exception;
	protected abstract void putBeanContent(BeanContent content) throws Exception;
	protected abstract void moveBeanContent(BeanContent content, String oldBizDataGroupId, String oldBizUserId) throws Exception;
	protected abstract void removeAttachmentContent(String contentId) throws Exception;
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
				AbstractPersistence.IMPLEMENTATION_CLASS = (Class<? extends AbstractPersistence>) Class.forName(UtilImpl.SKYVE_PERSISTENCE_CLASS);
			}
			catch (ClassNotFoundException e) {
				throw new IllegalStateException("Could not find SKYVE_PERSISTENCE_CLASS " + UtilImpl.SKYVE_PERSISTENCE_CLASS, e);
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
			cfg.put("hibernate.connection.driver_class", UtilImpl.DATA_STORE.getJdbcDriverClassName());
			cfg.put("hibernate.connection.url", UtilImpl.DATA_STORE.getJdbcUrl());
			String value = UtilImpl.DATA_STORE.getUserName();
			if (value != null) {
				cfg.put("hibernate.connection.username", value);
			}
			value = UtilImpl.DATA_STORE.getPassword();
			if (value != null) {
				cfg.put("hibernate.connection.password", value);
			}
			cfg.put("hibernate.connection.autocommit", "false");
		}
		else {
			cfg.put("hibernate.connection.datasource", dataSource);
		}
		cfg.put("hibernate.dialect", UtilImpl.DATA_STORE.getDialectClassName());

		// Query Caching screws up pessimistic locking
		cfg.put("hibernate.cache.use_query_cache", "false");

		// turn off second level caching (for now)
		cfg.put("hibernate.cache.use_second_level_cache", "false");
		cfg.put("hibernate.cache.provider_class", "org.hibernate.cache.EhCacheProvider");
		cfg.put("hibernate.cache.region.factory_class", "org.hibernate.cache.ehcache.EhCacheRegionFactory");
		
		// Allow more than 1 representation of the same detached entity to be merged,
		// possibly from multiple sessions, multiple caches, or various serializations.
		cfg.put("hibernate.event.merge.entity_copy_observer", "allow");
		
		// JDBC parameters
		cfg.put("hibernate.jdbc.use_streams_for_binary", "true");
		cfg.put("hibernate.jdbc.batch_size", "16");
		cfg.put("hibernate.max_fetch_depth", "3");

		if (UtilImpl.CATALOG != null) {
			cfg.put("hibernate.default_catalog", UtilImpl.CATALOG);
		}
		if (UtilImpl.SCHEMA != null) {
			cfg.put("hibernate.default_schema", UtilImpl.SCHEMA);
		}

		// Whether to generate dynamic proxies as classes or not (adds to classes loaded and thus Permanent Generation)
		cfg.put("hibernate.bytecode.use_reflection_optimizer", "false");

		// Update the database schema on first use
		if (UtilImpl.DDL_SYNC) {
			cfg.put("hibernate.hbm2ddl.auto", "update");
		}
		// The default of "grouped" may require hibernate.default_schema and/or hibernate.default_catalog to be provided.
		// Will have more luck with "individually".
		cfg.put("hibernate.hbm2ddl.jdbc_metadata_extraction_strategy", "individually");

		// Keep stats on usage
		cfg.put("hibernate.generate_statistics", "false");

		// Log SQL to stdout
		cfg.put("hibernate.show_sql", Boolean.toString(UtilImpl.SQL_TRACE));
		cfg.put("hibernate.format_sql", Boolean.toString(UtilImpl.PRETTY_SQL_OUTPUT));

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
				List<Integrator> result = new ArrayList<>();
				result.add(new Integrator() {
					@Override
					public void integrate(@SuppressWarnings("hiding") Metadata metadata,
											SessionFactoryImplementor sessionFactory,
											SessionFactoryServiceRegistry serviceRegistry) {
						HibernateListener listener = new HibernateListener();
						final EventListenerRegistry eventListenerRegistry = serviceRegistry.getService(EventListenerRegistry.class);

						// For CMS Update callbacks
						eventListenerRegistry.appendListeners(EventType.POST_UPDATE, listener);
						eventListenerRegistry.appendListeners(EventType.POST_INSERT, listener);

						// For BizLock and BizKey callbacks
						eventListenerRegistry.appendListeners(EventType.PRE_UPDATE, listener);

						// For ordering collection elements when initialised
						eventListenerRegistry.appendListeners(EventType.INIT_COLLECTION, listener);

						// For collection mutation callbacks
						// NB this didn't work - got the event name from the hibernate envers doco - maybe in a new version of hibernate
//						cfg.setListeners("pre-collection-update", new PreCollectionUpdateEventListener[] {hibernateListener});
//						cfg.setListeners("pre-collection-remove", new PreCollectionRemoveEventListener[] {hibernateListener});
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

		AbstractRepository repository = AbstractRepository.get();
		if (UtilImpl.USING_JPA) {
			// cfg.configure("bizhub", null);
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
					sources.addResource(mappingPath);
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
					sources.addResource(ormResourcePath);
				}
			}
		}

		metadata = sources.getMetadataBuilder().build();
		SessionFactoryBuilder sessionFactoryBuilder = metadata.getSessionFactoryBuilder();
		
		sf = sessionFactoryBuilder.build();

		if (UtilImpl.DDL_SYNC) {
			try {
				DDLDelegate.migrate(standardRegistry, metadata, AbstractHibernatePersistence.getDialect(), true);
			}
			catch (Exception e) {
				UtilImpl.LOGGER.severe("Could not apply skyve extra schema updates");
				e.printStackTrace();
			}
		}
	}

	public static SkyveDialect getDialect(String dialectClassName) {
		SkyveDialect dialect = DIALECTS.get(dialectClassName);
		if (dialect == null) {
			synchronized (AbstractHibernatePersistence.class) {
				dialect = DIALECTS.get(dialectClassName);
				if (dialect == null) {
					try {
						dialect = (SkyveDialect) Class.forName(dialectClassName).newInstance();
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
	
	public static SkyveDialect getDialect() {
		return getDialect(UtilImpl.DATA_STORE.getDialectClassName());
	}
	
	@Override
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

	private void treatPersistenceThrowable(Throwable t, OperationType operationType, PersistentBean bean) {
t.printStackTrace();
		if (t instanceof javax.persistence.OptimisticLockException) {
			if (bean.isPersisted()) {
				try {
					session.refresh(bean);
				}
				catch (@SuppressWarnings("unused") MappingException | IllegalArgumentException e) {
					// Cannot send in an entity name to refresh, so this happens when the object is transient or detached
					// So do nothing, we're about to throw Optimistic Lock anyway
				}
			}
			throw new OptimisticLockException(user.getCustomer(), operationType, bean.getBizLock());
		}
		else if (t instanceof StaleObjectStateException) {
			if (bean.isPersisted()) {
				try {
					session.refresh(bean);
				}
				catch (@SuppressWarnings("unused") MappingException | IllegalArgumentException e) {
					// Cannot send in an entity name to refresh, so this happens when the object is transient or detached
					// So do nothing, we're about to throw Optimistic Lock anyway
				}
			}
			throw new OptimisticLockException(user.getCustomer(), operationType, bean.getBizLock());
		}
		else if (t instanceof EntityNotFoundException) {
			throw new OptimisticLockException(user.getCustomer(), operationType, bean.getBizLock());
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
	public void setUser(User user) {
		super.setUser(user);
		resetDocumentPermissionScopes();
	}

	@Override
	public void setDocumentPermissionScopes(DocumentPermissionScope scope) {
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
	public void resetDocumentPermissionScopes() {
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
	private void setFilters(Document document, DocumentPermissionScope scope) {
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
	private void resetFilters(Document document) {
		DocumentPermissionScope scope = user.getScope(document.getOwningModuleName(), document.getName());
		setFilters(document, scope);
	}
	
	@Override
	public void setRollbackOnly() {
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
		if (em != null) {
			EntityTransaction et = em.getTransaction();
			if ((et != null) && et.isActive() && (! et.getRollbackOnly())) {
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
						// FROM THE HIBERNATE_REFERENCE DOCS Page 190
					    // Earlier versions of Hibernate required explicit disconnection and reconnection of a Session. 
					    // These methods are deprecated, as beginning and ending a transaction has the same effect.
					    et.commit();
					}
				}
			}
		}
		catch (@SuppressWarnings("unused") RollbackException e) {
			UtilImpl.LOGGER.warning("Cannot commit as transaction was rolled back earlier....");
		}
		finally {
			try {
				closeContent();
			}
			catch (Exception e) {
				UtilImpl.LOGGER.warning("Cannot commit content manager - " + e.getLocalizedMessage());
				e.printStackTrace();
			}
			finally {
				if (close) {
					if (em != null) { // can be null after a relogin
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
		if (session.contains(getDocumentEntityName(bean.getBizModule(), bean.getBizDocument()), bean)) {
			session.evict(bean);
		}
	}

	/**
	 * The refresh method is not on the Persistence interface as Hibernate can
	 * call et.setRollbackOnly() when exceptions are thrown by session.refresh().
	 * Use this with caution - if et.setRollbackOnly() is called there is no way to undo it
	 * and any calls to Persistence.commit() will just not work.
	 * @param bean
	 */
	public void refresh(Bean bean) {
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
	private void setMandatories(Document document, final Bean beanToSave) {
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
	public void preMerge(Document document, Bean beanToSave) {
		// set bizCustomer, bizLock & bizKey
		setMandatories(document, beanToSave);
		
		// Validate all and sundry before touching the database
		// Note that preSave events are all fired for the object graph and then validate for the graph is called.
		// This allows preSave events to mutate values and build more object graph nodes on before it is all validated.
		Customer customer = user.getCustomer();
		firePreSaveEvents(customer, document, beanToSave);
		validatePreMerge(customer, document, beanToSave);

		// set bizCustomer, bizLock & bizKey again in case 
		// more object hierarchy has been added during preSave()
		setMandatories(document, beanToSave);
	}

	private static void firePreSaveEvents(final Customer customer, Document document, final Bean beanToSave) {
		new BeanVisitor(false, false, false) {
			@Override
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
	
	private void validatePreMerge(final Customer customer, Document document, final Bean beanToSave) {
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
					ValidationUtil.validateBeanAgainstDocument(document, bean);

					Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
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
	
	@SuppressWarnings("unchecked")
	private <T extends PersistentBean> T save(Document document, T bean, boolean flush) {
		T result = null;
		
		try {
			CustomerImpl internalCustomer = (CustomerImpl) getUser().getCustomer();
			boolean vetoed = false;
			
			// We need to replace transient properties before calling postMerge as
			// Bizlet.postSave() implementations could manipulate these transients for display after save.
			try {
				vetoed = internalCustomer.interceptBeforeSave(document, bean);
				if (! vetoed) {
					preMerge(document, bean);
					String entityName = getDocumentEntityName(document.getOwningModuleName(), document.getName());
					result = (T) session.merge(entityName, bean);
					if (flush) {
						em.flush();
					}
				}
			}
			finally {
				if (result != null) { // only do if we got a result from the merge
					replaceTransientProperties(document, result, bean);
				}
			}
			if (! vetoed) {
				postMerge(document, result);
				internalCustomer.interceptAfterSave(document, result);
			}
		}
		catch (Throwable t) {
			treatPersistenceThrowable(t, OperationType.update, bean);
		}

		return result;
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

	@SuppressWarnings("unchecked")
	private <T extends PersistentBean> List<T> save(List<T> beans, boolean flush) {
		List<T> results = new ArrayList<>();
		PersistentBean currentBean = null; // used in exception handling
		
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
					for (PersistentBean bean : beans) {
						currentBean = bean; // for exception handling
						Module m = internalCustomer.getModule(bean.getBizModule());
						Document d = m.getDocument(internalCustomer, bean.getBizDocument());
						preMerge(d, bean);
					}
					
					for (PersistentBean bean : beans) {
						currentBean = bean; // for exception handling
						Module m = internalCustomer.getModule(bean.getBizModule());
						Document d = m.getDocument(internalCustomer, bean.getBizDocument());

						String entityName = getDocumentEntityName(d.getOwningModuleName(), d.getName());
						results.add((T) session.merge(entityName, bean));
					}
					if (flush) {
						em.flush();
					}
				}
			}
			finally {
				int i = 0;
				for (PersistentBean result : results) {
					currentBean = result; // for exception handling
					if (result != null) { // only do if we got a result from the merge
						PersistentBean bean = beans.get(i);
						Module m = internalCustomer.getModule(bean.getBizModule());
						Document d = m.getDocument(internalCustomer, bean.getBizDocument());
						replaceTransientProperties(d, result, bean);
					}
					i++;
				}
			}
			if (! vetoed) {
				for (PersistentBean result : results) {
					currentBean = result; // for exception handling
					Module m = internalCustomer.getModule(result.getBizModule());
					Document d = m.getDocument(internalCustomer, result.getBizDocument());
					postMerge(d, result);
				}
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

		return results;
	}
	
	@Override
	public void postMerge(Document document, final Bean beanToSave) {
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

	@Override
	public void replaceTransientProperties(Document document, final Bean savedBean, final Bean unmergedBean) {
		Customer customer = user.getCustomer();

		new BeanVisitor(false, false, false) {
			@Override
			protected boolean accept(String binding,
										@SuppressWarnings("hiding") Document document,
										Document parentDocument,
										Relation parentRelation,
										Bean bean) {
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
	 */
	private void checkUniqueConstraints(Document document, Bean bean) {
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
				int i = 1;
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
					queryString.append(" = ?").append(i++);
				}
	
				if (abortCheck) {
					continue; // iterate to next constraint
				}

				Query<?> query = session.createQuery(queryString.toString());
				if (UtilImpl.QUERY_TRACE) {
					StringBuilder log = new StringBuilder(256);
					log.append("TEST CONSTRAINT ").append(owningModuleName).append('.').append(documentName).append('.').append(constraint.getName());
					log.append(" using ").append(queryString);
					Util.LOGGER.info(log.toString());
				}
				query.setLockMode("bean", LockMode.READ); // take a read lock on all referenced documents
				int index = 1;
				for (@SuppressWarnings("unused") String fieldName : constraint.getFieldNames()) {
					Object value = constraintFieldValues.get(index - 1);
					query.setParameter(index, value);
					if (UtilImpl.QUERY_TRACE) {
						Util.LOGGER.info("    SET PARAM " + index + " = " + value);
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
							String message = null;
							try {
								message = BindUtil.formatMessage(customer, constraint.getMessage(), bean);
							}
							catch (Exception e) {
								e.printStackTrace();
								message = "Unique Constraint Violation occurred but could not display the unique constraint message for constraint " +
												constraint.getName();
							}
	
							throw new UniqueConstraintViolationException(document, constraint.getName(), message);
						}
					}
				}
			}
		}
		finally {
			resetFilters(document);
		}
	}

	/**
	 * This is a stack so that delete operations called in preDelete bizlet methods will work correctly.
	 * 
	 * The beanToDelete is put into the Map under the key of "" with a singleton set.
	 * The beanToDelete is used to detect the same event in preRemove.
	 * 
	 * Other beans that will base cascaded are put using 
	 * entity name -> set of beans that are being deleted during the delete operation.
	 * This attribute holds all beans being deleted during the delete operation, by entity name,
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
	@SuppressWarnings("unchecked")
	public final <T extends PersistentBean> void delete(Document document, T bean) {
		Map<String, Set<Bean>> beansToDelete = new TreeMap<>();
		T beanToDelete = bean;
		
		if (isPersisted(beanToDelete)) {
			try {
				CustomerImpl internalCustomer = (CustomerImpl) getUser().getCustomer();
				boolean vetoed = internalCustomer.interceptBeforeDelete(document, beanToDelete);
				if (! vetoed) {
					// need to merge before validation to ensure that the FK constraints
					// can check for members of collections etc - need the persistent version for this
					String entityName = getDocumentEntityName(document.getOwningModuleName(), document.getName());
					beanToDelete = (T) session.merge(entityName, beanToDelete);
					em.flush();
					UtilImpl.populateFully(beanToDelete);
	
					// Push a new delete context on
					beansToDelete.put("", Collections.singleton(beanToDelete));
					deleteContext.push(beansToDelete);
					
					// Call preDelete()
					Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(internalCustomer);
					if (bizlet != null) {
						if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preDelete", "Entering " + bizlet.getClass().getName() + ".preDelete: " + beanToDelete);
						bizlet.preDelete(beanToDelete);
						if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preDelete", "Exiting " + bizlet.getClass().getName() + ".preDelete");
					}
					
					Set<String> documentsVisited = new TreeSet<>();
					// Check composed collections/associations here in case we are 
					// deleting a composed collection element or an association directly using p.delete().
					checkReferentialIntegrityOnDelete(document,
														beanToDelete,
														documentsVisited,
														beansToDelete,
														false);

					session.delete(entityName, beanToDelete);
					em.flush();
				
					internalCustomer.interceptAfterDelete(document, beanToDelete);
				}
			}
			catch (Throwable t) {
				treatPersistenceThrowable(t, OperationType.update, beanToDelete);
			}
			finally {
				beansToDelete.clear();
				deleteContext.pop();
			}
		}
	}

	// Do not increase visibility of this method as we don't want it to be public.
	private void checkReferentialIntegrityOnDelete(Document document, 
													PersistentBean bean, 
													Set<String> documentsVisited,
													Map<String, Set<Bean>> beansToBeCascaded,
													boolean preRemove) {
		Customer customer = user.getCustomer();
		List<ExportedReference> refs = ((CustomerImpl) customer).getExportedReferences(document);
		if (refs != null) {
			for (ExportedReference ref : refs) {
				ReferenceType type = ref.getType();
				// Need to check aggregation FKs
				// Need to check collection joining table element_id FKs
				// but do NOT need to check child collection parent_ids as they point back
				if (! CollectionType.child.equals(type)) {
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
						String entityName = getDocumentEntityName(moduleName, documentName);
						Module referenceModule = customer.getModule(moduleName);
						Document referenceDocument = referenceModule.getDocument(customer, documentName);
						Persistent persistent = document.getPersistent();
						if (persistent != null) {
							if (ExtensionStrategy.mapped.equals(persistent.getStrategy())) {
								checkMappedReference(bean, beansToBeCascaded, document, ref, entityName, referenceDocument);
							}
							else {
								checkTypedReference(bean, beansToBeCascaded, document, ref, entityName, referenceDocument);
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

	private void checkTypedReference(PersistentBean bean, 
										Map<String, Set<Bean>> beansToBeCascaded,
										Document document,
										ExportedReference ref,
										String entityName,
										Document referenceDocument) {
		if (ExtensionStrategy.mapped.equals(referenceDocument.getPersistent().getStrategy())) {
			// Find all implementations below the mapped and check these instead
			Set<Document> derivations = new HashSet<>();
			populateImmediateMapImplementingDerivations((CustomerImpl) user.getCustomer(), referenceDocument, derivations);
			for (Document derivation : derivations) {
				checkTypedReference(bean, beansToBeCascaded, document, ref, entityName, derivation);
			}
		}
		else {
			setFilters(referenceDocument, DocumentPermissionScope.global);
			try {
				StringBuilder queryString = new StringBuilder(64);
				queryString.append("select bean from ");
				// NB Don't ever change this to the entityName parameter as this method can be called recursively above.
				//    The entityName is used to find cascaded beans to exclude from the integrity test
				queryString.append(getDocumentEntityName(referenceDocument.getOwningModuleName(), referenceDocument.getName()));
				queryString.append(" as bean");
				if (ref.isCollection()) {
					// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
					queryString.append(" where :referencedBeanId member of bean.");
					queryString.append(ref.getReferenceFieldName());
				}
				else {
					queryString.append(" where bean.");
					queryString.append(ref.getReferenceFieldName());
					// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
					queryString.append(".bizId = :referencedBeanId");
				}
				
				Set<Bean> theseBeansToBeCascaded = beansToBeCascaded.get(entityName);
				if (theseBeansToBeCascaded != null) {
					int i = 0;
					for (@SuppressWarnings("unused") Bean thisBeanToBeCascaded : theseBeansToBeCascaded) {
						// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
						queryString.append(" and bean.bizId != :deletedBeanId").append(i++);
					}
				}
				if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("FK check : " + queryString);
	
				Query<?> query = session.createQuery(queryString.toString());
				query.setLockMode("bean", LockMode.READ); // read lock required for referential integrity
				// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
				query.setParameter("referencedBeanId", bean.getBizId(), StringType.INSTANCE);
				if (theseBeansToBeCascaded != null) {
					int i = 0;
					for (Bean thisBeanToBeCascaded : theseBeansToBeCascaded) {
						// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
						query.setParameter("deletedBeanId" + i++, thisBeanToBeCascaded.getBizId(), StringType.INSTANCE);
					}
				}
	
				try (ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY)) {
					if (results.next()) {
						throw new ReferentialConstraintViolationException("Cannot delete " + document.getSingularAlias() + 
																			" \"" + bean.getBizKey() + 
																			"\" as it is referenced by a " + ref.getDocumentAlias());
					}
				}
			}
			finally {
				resetFilters(referenceDocument);
			}
		}
	}
	
	private void checkMappedReference(PersistentBean bean, 
										Map<String, Set<Bean>> beansToBeCascaded,
										Document document,
										ExportedReference ref,
										String entityName,
										Document referenceDocument) {
		if (ExtensionStrategy.mapped.equals(referenceDocument.getPersistent().getStrategy())) {
			// Find all implementations below the mapped and check these instead
			Set<Document> derivations = new HashSet<>();
			populateImmediateMapImplementingDerivations((CustomerImpl) user.getCustomer(), referenceDocument, derivations);
			for (Document derivation : derivations) {
				checkMappedReference(bean, beansToBeCascaded, document, ref, entityName, derivation);
			}
		}
		else {
			StringBuilder queryString = new StringBuilder(64);
			queryString.append("select 1 from ");
			queryString.append(referenceDocument.getPersistent().getPersistentIdentifier());
			if (ref.isCollection()) {
				queryString.append('_').append(ref.getReferenceFieldName());
				queryString.append(" where ").append(PersistentBean.ELEMENT_COLUMN_NAME).append(" = :reference_id");
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
						queryString.append(" and ").append(PersistentBean.OWNER_COLUMN_NAME).append(" != :deleted_id");
					}
					else {
						queryString.append(" and ").append(Bean.DOCUMENT_ID).append(" != :deleted_id");
					}
					queryString.append(i++);
				}
			}
			if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("FK check : " + queryString);
	
			NativeQuery<?> query = session.createNativeQuery(queryString.toString());
//			query.setLockMode("bean", LockMode.READ); // read lock required for referential integrity
			query.setParameter("reference_id", bean.getBizId(), StringType.INSTANCE);
			if (theseBeansToBeCascaded != null) {
				int i = 0;
				for (Bean thisBeanToBeCascaded : theseBeansToBeCascaded) {
					query.setParameter("deleted_id" + i++, thisBeanToBeCascaded.getBizId(), StringType.INSTANCE);
				}
			}
	
			try (ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY)) {
				if (results.next()) {
					throw new ReferentialConstraintViolationException("Cannot delete " + document.getSingularAlias() + 
																		" \"" + bean.getBizKey() + 
																		"\" as it is referenced by a " + ref.getDocumentAlias());
				}
			}
		}
	}

	private void populateImmediateMapImplementingDerivations(CustomerImpl customer,
																Document document,
																Set<Document> result) {
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
	public <T extends Bean> T retrieve(Document document, String id) {
		return retrieve(document, id, false);
	}
	
	@Override
	public <T extends Bean> T retrieveAndLock(Document document, String id) {
		return retrieve(document, id, true);
	}
	
	@SuppressWarnings("unchecked")
	private final <T extends Bean> T retrieve(Document document, String id, boolean forUpdate) {
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
		catch (@SuppressWarnings("unused") StaleObjectStateException e) // thrown from session.load() with LockMode.UPGRADE
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

			session.refresh(result, LockMode.PESSIMISTIC_WRITE);
		}
		catch (ClassNotFoundException e) {
			throw new MetaDataException("Could not find bean", e);
		}

		return result;
	}

	@Override
	public void postLoad(AbstractPersistentBean loadedBean)
	throws Exception {
		// Inject any dependencies
		BeanProvider.injectFields(loadedBean);

		Customer customer = user.getCustomer();
		Module module = customer.getModule(loadedBean.getBizModule());
		Document document = module.getDocument(customer, loadedBean.getBizDocument());
		
		// check that embedded objects are empty and null them if they are
		nullEmbeddedReferencesOnLoad(customer, module, document, loadedBean);
		
		CustomerImpl internalCustomer = (CustomerImpl) customer;
		boolean vetoed = internalCustomer.interceptBeforePostLoad(loadedBean);
		if (! vetoed) {
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

	private static void nullEmbeddedReferencesOnLoad(Customer customer,
														Module module,
														Document document,
														AbstractPersistentBean loadedBean) {
		for (Attribute attribute : document.getAllAttributes()) {
			if (attribute instanceof Association) {
				Association association = (Association) attribute;
				if (AssociationType.embedded.equals(association.getType())) {
					String embeddedName = association.getName();
					Bean embeddedBean = (Bean) BindUtil.get(loadedBean, embeddedName);
					if (embeddedBean != null) {
						Document embeddedDocument = module.getDocument(customer, association.getDocumentName());
						boolean empty = true;
						for (Attribute embeddedAttribute : embeddedDocument.getAllAttributes()) {
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
						if (! propertyTypes[i].isEqual(state[i], oldState[i])) {
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
				if (AttributeType.content.equals(type) || AttributeType.image.equals(type)) {
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
	public void preRemove(AbstractPersistentBean bean)
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
		}.visit(document, bean, customer);
		
		try {
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePreDelete(bean);
			if (! vetoed) {
				Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preDelete", "Entering " + bizlet.getClass().getName() + ".preDelete: " + bean);
					bizlet.preDelete(bean);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preDelete", "Exiting " + bizlet.getClass().getName() + ".preDelete");
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
			((PersistentBean) bean).setBizLock(new OptimisticLock(user.getName(), new Date()));
		}
		catch (ValidationException e) {
			for (Message message : e.getMessages()) {
				ValidationUtil.processMessageBindings(customer, message, bean, bean);
			}
			throw e;
		}
	}

	@Override
	public void postRemove(AbstractPersistentBean loadedBean)
	throws Exception {
		removeBeanContent(loadedBean);
	}
	
	public final Connection getConnection() {
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
	
	private static final Integer NEW_VERSION = new Integer(0);
	private static final String CHILD_PARENT_ID = ChildBean.PARENT_NAME + "_id";
	
	@Override
	public void upsertBeanTuple(PersistentBean bean) {
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
			query.append(',').append(Bean.CUSTOMER_NAME).append("=:").append(Bean.CUSTOMER_NAME);
			query.append(',').append(Bean.DATA_GROUP_ID).append("=:").append(Bean.DATA_GROUP_ID);
			query.append(',').append(Bean.USER_ID).append("=:").append(Bean.USER_ID);
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

					// If this is an arc, add the type column to the insert
					Association association = (Association) attribute;
					String referencedDocumentName = association.getDocumentName();
					Document referencedDocument = module.getDocument(customer, referencedDocumentName);
					Persistent referencedPersistent = referencedDocument.getPersistent();
					if ((referencedPersistent != null) && ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy())) {
						query.append(',').append(attributeName).append("_type=:").append(attributeName).append("_type");
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

					// If this is an arc, add the type column to the insert
					Association association = (Association) attribute;
					String referencedDocumentName = association.getDocumentName();
					Document referencedDocument = module.getDocument(customer, referencedDocumentName);
					Persistent referencedPersistent = referencedDocument.getPersistent();
					if ((referencedPersistent != null) && ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy())) {
						columns.append(',').append(attributeName).append("_type");
						values.append(",:").append(attributeName).append("_type");
					}
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
		}
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
				sql.putParameter(CHILD_PARENT_ID, (parent == null) ? null : parent.getBizId(), false);
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

					// If this is an arc, add the type column to the insert
					Association association = (Association) attribute;
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
	public void upsertCollectionTuples(PersistentBean owningBean, String collectionName) {
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
			query.append(" where ").append(PersistentBean.OWNER_COLUMN_NAME).append("=:");
			query.append(PersistentBean.OWNER_COLUMN_NAME).append(" and ").append(PersistentBean.ELEMENT_COLUMN_NAME);
			query.append("=:").append(PersistentBean.ELEMENT_COLUMN_NAME);

			SQL sql = newSQL(query.toString());
			sql.putParameter(PersistentBean.OWNER_COLUMN_NAME, owningBean.getBizId(), false);
			sql.putParameter(PersistentBean.ELEMENT_COLUMN_NAME, elementBean.getBizId(), false);

			boolean notExists = sql.tupleResults().isEmpty();
			query.setLength(0);
			if (notExists) {
				query.append("insert into ").append(document.getPersistent().getPersistentIdentifier()).append('_').append(collectionName);
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
	
	@Override
	public void insertCollectionTuples(PersistentBean owningBean, String collectionName) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(owningBean.getBizModule());
		Document document = module.getDocument(customer, owningBean.getBizDocument());
		StringBuilder query = new StringBuilder(256);
		query.append("insert into ").append(document.getPersistent().getPersistentIdentifier()).append('_').append(collectionName);
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
		
		for (Bean elementBean : elementBeans) {
			SQL sql = newSQL(query.toString());
			sql.putParameter(PersistentBean.OWNER_COLUMN_NAME, owningBean.getBizId(), false);
			sql.putParameter(PersistentBean.ELEMENT_COLUMN_NAME, elementBean.getBizId(), false);

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
	public SQL newNamedSQL(String moduleName, String queryName) {
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
	public SQL newNamedSQL(String moduleName, String documentName, String queryName) {
		Module module = user.getCustomer().getModule(moduleName);
		return new HibernateSQL(moduleName, documentName, module.getSQL(queryName).getQuery(), this);
	}

	@Override
	public SQL newNamedSQL(Document document, String queryName) {
		Module module = user.getCustomer().getModule(document.getOwningModuleName());
		return new HibernateSQL(document, module.getSQL(queryName).getQuery(), this);
	}

	@Override
	public BizQL newBizQL(String query) {
		return new HibernateBizQL(query, this);
	}

	@Override
	public BizQL newNamedBizQL(String moduleName, String queryName) {
		Module module = user.getCustomer().getModule(moduleName);
		return new HibernateBizQL(module.getBizQL(queryName).getQuery(), this);
	}

	@Override
	public BizQL newNamedBizQL(Module module, String queryName) {
		return new HibernateBizQL(module.getBizQL(queryName).getQuery(), this);
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
	public DocumentQuery newDocumentQuery(Document document, String fromClause, String filterClause) {
		return new HibernateDocumentQuery(document, fromClause, filterClause, this);
	}

	@Override
	public DocumentQuery newDocumentQuery(Bean queryByExampleBean)
	throws Exception {
		return new HibernateDocumentQuery(queryByExampleBean, this);
	}
}
