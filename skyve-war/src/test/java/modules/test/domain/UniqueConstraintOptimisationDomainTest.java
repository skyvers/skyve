package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class UniqueConstraintOptimisationDomainTest {

	private UniqueConstraintOptimisation bean;

	@BeforeEach
	void setUp() {
		bean = new UniqueConstraintOptimisation();
	}

	@Test
	void bizModuleIsTest() {
		assertEquals("test", bean.getBizModule());
	}

	@Test
	void bizDocumentIsUniqueConstraintOptimisation() {
		assertEquals("UniqueConstraintOptimisation", bean.getBizDocument());
	}

	@Test
	void getBizKeyNotNull() {
		assertNotNull(bean.getBizKey());
	}

	@Test
	void uc1SetAndGet() {
		bean.setUc1("value1");
		assertEquals("value1", bean.getUc1());
	}

	@Test
	void uc2SetAndGet() {
		bean.setUc2("value2");
		assertEquals("value2", bean.getUc2());
	}

	@Test
	void uc3SetAndGet() {
		bean.setUc3("value3");
		assertEquals("value3", bean.getUc3());
	}

	@Test
	void nonPersistentAssociationNullByDefault() {
		assertNull(bean.getNonPersistentAssociation());
	}

	@Test
	void persistentAssociationNullByDefault() {
		assertNull(bean.getPersistentAssociation());
	}

        @Test
        void nonPersistentAssociationSetAndGet() {
                UniqueConstraintOptimisation assoc = new UniqueConstraintOptimisation();
                bean.setNonPersistentAssociation(assoc);
                assertEquals(assoc, bean.getNonPersistentAssociation());
                // Setting same value is a no-op
                bean.setNonPersistentAssociation(assoc);
                assertEquals(assoc, bean.getNonPersistentAssociation());
        }

        @Test
        void persistentAssociationSetAndGet() {
                UniqueConstraintOptimisation assoc = new UniqueConstraintOptimisation();
                bean.setPersistentAssociation(assoc);
                assertEquals(assoc, bean.getPersistentAssociation());
                // Setting same value is a no-op
                bean.setPersistentAssociation(assoc);
                assertEquals(assoc, bean.getPersistentAssociation());
        }
}
