package org.skyve.impl.persistence;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;

@SuppressWarnings({"static-method", "boxing"})
class RDBMSDynamicPersistenceTest {
	@Test
	void postConstructStoresOwningPersistenceAndLifecycleMethodsAreNoOps() throws Exception {
		try (RDBMSDynamicPersistence dynamicPersistence = new RDBMSDynamicPersistence()) {
			Persistence persistence = mock(Persistence.class);

			dynamicPersistence.postConstruct(persistence);
			dynamicPersistence.begin();
			dynamicPersistence.commit();
			dynamicPersistence.rollback();

			assertSame(persistence, persistenceField().get(dynamicPersistence));
		}
	}

	@Test
	void cacheMethodsTrackAndEvictDynamicBeansByBizId() throws Exception {
		try (RDBMSDynamicPersistence dynamicPersistence = new RDBMSDynamicPersistence()) {
			DynamicPersistentBean cachedBean = mock(DynamicPersistentBean.class);
			Bean lookupBean = mock(Bean.class);
			when(cachedBean.getBizId()).thenReturn("dynamic-1");
			when(lookupBean.getBizId()).thenReturn("dynamic-1");
			cache(dynamicPersistence).put("dynamic-1", cachedBean);

			assertTrue(dynamicPersistence.cached(lookupBean));
			dynamicPersistence.evictCached(lookupBean);
			assertFalse(dynamicPersistence.cached(lookupBean));

			cache(dynamicPersistence).put("dynamic-1", cachedBean);
			dynamicPersistence.evictAllCached();
			assertFalse(dynamicPersistence.cached(lookupBean));
		}
	}

	@Test
	void insertEntityBuildsDynamicEntityInsertAndBindsBeanValues() throws Exception {
		try (RDBMSDynamicPersistence dynamicPersistence = new RDBMSDynamicPersistence()) {
			Persistence persistence = mock(Persistence.class);
			User user = mock(User.class);
			SQL sql = fluentSQL();
			PersistentBean bean = mock(PersistentBean.class);
			when(persistence.getUser()).thenReturn(user);
			when(user.getName()).thenReturn("admin");
			when(persistence.newSQL("insert into ADM_DynamicEntity (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizFlagComment, bizDataGroupId, bizUserId, moduleName, documentName, fields) "
					+ "values (:bizId, :bizVersion, :bizLock, :bizKey, :bizCustomer, :bizFlagComment, :bizDataGroupId, :bizUserId, :moduleName, :documentName, :fields)"))
							.thenReturn(sql);
			when(bean.getBizId()).thenReturn("dynamic-1");
			when(bean.getBizVersion()).thenReturn(null, Integer.valueOf(0));
			when(bean.getBizKey()).thenReturn(null);
			when(bean.getBizCustomer()).thenReturn("demo");
			when(bean.getBizFlagComment()).thenReturn("flag");
			when(bean.getBizDataGroupId()).thenReturn("group");
			when(bean.getBizUserId()).thenReturn("user");
			when(bean.getBizModule()).thenReturn("admin");
			when(bean.getBizDocument()).thenReturn("DynamicDoc");
			dynamicPersistence.postConstruct(persistence);
			setField(dynamicPersistence, "dynamicEntityPersistentIdentifier", "ADM_DynamicEntity");

			invokeInsertEntity(dynamicPersistence, bean, "{\"name\":\"value\"}");

			verify(bean).setBizVersion(Integer.valueOf(0));
			verify(sql).putParameter(Bean.DOCUMENT_ID, "dynamic-1", false);
			verify(sql).putParameter(PersistentBean.VERSION_NAME, Integer.valueOf(0));
			verify(sql).putParameter(Bean.BIZ_KEY, "Unkown", false);
			verify(sql).putParameter("moduleName", "admin", false);
			verify(sql).putParameter("documentName", "DynamicDoc", false);
			verify(sql).putParameter("fields", "{\"name\":\"value\"}", true);
			verify(sql).execute();
		}
	}

	@Test
	void insertReferencesWritesCollectionAndEmbeddedAssociationRows() throws Exception {
		try (RDBMSDynamicPersistence dynamicPersistence = new RDBMSDynamicPersistence()) {
			Persistence persistence = mock(Persistence.class);
			User user = mock(User.class);
			SQL sql = fluentSQL();
			Customer customer = mock(Customer.class);
			Module module = mock(Module.class);
			Document document = mock(Document.class);
			when(persistence.getUser()).thenReturn(user);
			when(user.getName()).thenReturn("admin");
			when(persistence.newSQL("insert into ADM_DynamicRelation (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizFlagComment, bizDataGroupId, bizUserId, parent_id, relatedModuleName, relatedDocumentName, relatedId, attributeName, ordinal) "
					+ "values (:bizId, 0, :bizLock, :bizKey, :bizCustomer, null, null, :bizUserId, :parent_id, :relatedModuleName, :relatedDocumentName, :relatedId, :attributeName, :ordinal)"))
							.thenReturn(sql);
			when(customer.getModule("sales")).thenReturn(module);
			when(module.getDocument(customer, "Order")).thenReturn(document);
			when(document.isDynamic()).thenReturn(false);
			DynamicPersistentBean owner = new DynamicPersistentBean("admin", "Owner", new TreeMap<>());
			owner.putDynamic(Bean.DOCUMENT_ID, "owner-1");
			owner.putDynamic(Bean.CUSTOMER_NAME, "demo");
			owner.putDynamic(Bean.USER_ID, "user-1");
			DynamicBean collectionRelated = new DynamicBean("sales", "Order", new TreeMap<>());
			collectionRelated.putDynamic(Bean.DOCUMENT_ID, "order-1");
			DynamicBean embeddedRelated = new DynamicBean("admin", "Embedded", new TreeMap<>());
			embeddedRelated.putDynamic(Bean.DOCUMENT_ID, "embedded-1");
			owner.putDynamic("items", List.of(collectionRelated));
			owner.putDynamic("embedded", embeddedRelated);
			Map<String, Boolean> references = new TreeMap<>();
			references.put("items", Boolean.FALSE);
			references.put("embedded", Boolean.TRUE);
			dynamicPersistence.postConstruct(persistence);
			setField(dynamicPersistence, "dynamicRelationPersistentIdentifier", "ADM_DynamicRelation");

			invokeInsertReferences(dynamicPersistence, customer, owner, references);

			verify(sql).putParameter("parent_id", "owner-1", false);
			verify(sql).putParameter(Bean.BIZ_KEY, "owner-1->order-1", false);
			verify(sql).putParameter("relatedModuleName", "sales", false);
			verify(sql).putParameter("relatedDocumentName", "Order", false);
			verify(sql).putParameter("relatedId", "order-1", false);
			verify(sql).putParameter("attributeName", "items", false);
			verify(sql).putParameter("ordinal", Integer.valueOf(0));
			verify(sql).putParameter(Bean.BIZ_KEY, "owner-1->embedded-1", false);
			verify(sql).putParameter("relatedModuleName", (String) null, false);
			verify(sql).putParameter("relatedDocumentName", (String) null, false);
			verify(sql).putParameter("relatedId", "embedded-1", false);
			verify(sql).putParameter("attributeName", "embedded", false);
			verify(sql).putParameter("ordinal", null, false);
			verify(sql, times(2)).execute();
		}
	}

	private static SQL fluentSQL() {
		SQL sql = mock(SQL.class);
		when(sql.putParameter(anyString(), any(), anyBoolean())).thenReturn(sql);
		return sql;
	}

	private static void invokeInsertEntity(RDBMSDynamicPersistence dynamicPersistence, PersistentBean bean, String json)
	throws Exception {
		Method method = RDBMSDynamicPersistence.class.getDeclaredMethod("insertEntity", PersistentBean.class, String.class);
		method.setAccessible(true);
		method.invoke(dynamicPersistence, bean, json);
	}

	private static void invokeInsertReferences(RDBMSDynamicPersistence dynamicPersistence,
												Customer customer,
												PersistentBean bean,
												Map<String, Boolean> references)
	throws Exception {
		Method method = RDBMSDynamicPersistence.class.getDeclaredMethod("insertReferences", Customer.class, PersistentBean.class, Map.class);
		method.setAccessible(true);
		method.invoke(dynamicPersistence, customer, bean, references);
	}

	@SuppressWarnings("unchecked")
	private static Map<String, DynamicPersistentBean> cache(RDBMSDynamicPersistence dynamicPersistence) throws Exception {
		Field field = RDBMSDynamicPersistence.class.getDeclaredField("dynamicFirstLevelCache");
		field.setAccessible(true);
		return (Map<String, DynamicPersistentBean>) field.get(dynamicPersistence);
	}

	private static Field persistenceField() throws Exception {
		Field field = RDBMSDynamicPersistence.class.getDeclaredField("persistence");
		field.setAccessible(true);
		return field;
	}

	private static void setField(RDBMSDynamicPersistence dynamicPersistence, String name, Object value) throws Exception {
		Field field = RDBMSDynamicPersistence.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(dynamicPersistence, value);
	}
}
