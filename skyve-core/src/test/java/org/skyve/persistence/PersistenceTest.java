package org.skyve.persistence;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.withSettings;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.ModuleDocument;
import org.skyve.metadata.module.query.ModuleBizQL;
import org.skyve.metadata.module.query.ModuleQuery;
import org.skyve.metadata.module.query.ModuleSQL;

/**
 * Tests that the typed-reference default methods of {@link Persistence} delegate to the String
 * forms with the reference's module name and metadata name.
 */
@SuppressWarnings("static-method")
class PersistenceTest {
	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class PersistenceDouble implements Persistence {
		private static final long serialVersionUID = 1L;
	}

	private enum TestDocument implements ModuleDocument {
		CONTACT;

		@Override
		public String moduleName() {
			return "admin";
		}

		@Override
		public String documentName() {
			return "Contact";
		}
	}

	private enum TestQuery implements ModuleQuery {
		Q_CONTACTS;

		@Override
		public String moduleName() {
			return "admin";
		}

		@Override
		public String queryName() {
			return "qContacts";
		}
	}

	private enum TestBizQL implements ModuleBizQL {
		B_CONTACTS;

		@Override
		public String moduleName() {
			return "admin";
		}

		@Override
		public String queryName() {
			return "bContacts";
		}
	}

	private enum TestSQL implements ModuleSQL {
		S_CONTACTS;

		@Override
		public String moduleName() {
			return "admin";
		}

		@Override
		public String queryName() {
			return "sContacts";
		}
	}

	private static Persistence mockPersistence() {
		return mock(PersistenceDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
	}

	@Test
	void queryFactoriesDelegateToTheStringForms() {
		Persistence p = mockPersistence();
		DocumentQuery documentQuery = mock(DocumentQuery.class);
		DocumentQuery namedQuery = mock(DocumentQuery.class);
		BizQL bizQL = mock(BizQL.class);
		SQL sql = mock(SQL.class);
		doReturn(documentQuery).when(p).newDocumentQuery("admin", "Contact");
		doReturn(namedQuery).when(p).newNamedDocumentQuery("admin", "qContacts");
		doReturn(bizQL).when(p).newNamedBizQL("admin", "bContacts");
		doReturn(sql).when(p).newNamedSQL("admin", "sContacts");

		assertAll(() -> assertSame(documentQuery, p.newDocumentQuery(TestDocument.CONTACT)),
					() -> assertSame(namedQuery, p.newNamedDocumentQuery(TestQuery.Q_CONTACTS)),
					() -> assertSame(bizQL, p.newNamedBizQL(TestBizQL.B_CONTACTS)),
					() -> assertSame(sql, p.newNamedSQL(TestSQL.S_CONTACTS)));
	}

	@Test
	void retrievesDelegateToTheStringForms() {
		Persistence p = mockPersistence();
		Bean retrieved = mock(Bean.class);
		Bean locked = mock(Bean.class);
		doReturn(retrieved).when(p).retrieve("admin", "Contact", "id1");
		doReturn(locked).when(p).retrieveAndLock("admin", "Contact", "id1");

		assertAll(() -> assertSame(retrieved, p.<Bean>retrieve(TestDocument.CONTACT, "id1")),
					() -> assertSame(locked, p.<Bean>retrieveAndLock(TestDocument.CONTACT, "id1")));
	}

	@Test
	void sharedCacheChecksDelegateToTheStringForms() {
		Persistence p = mockPersistence();
		doReturn(Boolean.TRUE).when(p).sharedCacheBean("admin", "Contact", "id1");
		doReturn(Boolean.TRUE).when(p).sharedCacheCollection("admin", "Contact", "roles", "id1");

		assertAll(() -> assertTrue(p.sharedCacheBean(TestDocument.CONTACT, "id1")),
					() -> assertTrue(p.sharedCacheCollection(TestDocument.CONTACT, "roles", "id1")));
	}

	@Test
	void sharedCacheEvictionsDelegateToTheStringForms() {
		Persistence p = mockPersistence();

		p.evictSharedCacheBeans(TestDocument.CONTACT);
		p.evictSharedCachedBean(TestDocument.CONTACT, "id1");
		p.evictSharedCacheCollections(TestDocument.CONTACT, "roles");
		p.evictSharedCacheCollection(TestDocument.CONTACT, "roles", "id1");

		assertAll(() -> verify(p).evictSharedCacheBeans("admin", "Contact"),
					() -> verify(p).evictSharedCachedBean("admin", "Contact", "id1"),
					() -> verify(p).evictSharedCacheCollections("admin", "Contact", "roles"),
					() -> verify(p).evictSharedCacheCollection("admin", "Contact", "roles", "id1"));
	}
}
