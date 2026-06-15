package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UniqueConstraintMultipleNavigableDomainTest extends AbstractH2Test {

	@Test
	void bizModuleIsTest() {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		assertEquals("test", bean.getBizModule());
	}

	@Test
	void bizDocumentIsUniqueConstraintMultipleNavigable() {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		assertEquals("UniqueConstraintMultipleNavigable", bean.getBizDocument());
	}

	@Test
	void getBizKeyNotNull() {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void uniqueNameSetAndGet() {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		bean.setUniqueName("testName");
		assertEquals("testName", bean.getUniqueName());
	}

	@Test
	void aggAssociationNullByDefault() {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		assertNull(bean.getAggAssociation());
	}

	@Test
	void aggAssociationSetAndGet() {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		UniqueConstraintMultipleNavigable assoc = UniqueConstraintMultipleNavigable.newInstance();
		bean.setAggAssociation(assoc);
		assertEquals(assoc, bean.getAggAssociation());
	}

	@Test
	void settingAggAssociationSetsInverseOnAssoc() {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		UniqueConstraintMultipleNavigable assoc = UniqueConstraintMultipleNavigable.newInstance();
		bean.setAggAssociation(assoc);
		assertEquals(bean, assoc.getInvAggAssociation());
	}

	@Test
	void nullAggAssociationClearsField() {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		UniqueConstraintMultipleNavigable assoc = UniqueConstraintMultipleNavigable.newInstance();
		bean.setAggAssociation(assoc);
		bean.nullAggAssociation();
		assertNull(bean.getAggAssociation());
	}

	@Test
	void invAggAssociationNullByDefault() {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		assertNull(bean.getInvAggAssociation());
	}

	@Test
	void nullInvAggAssociationClearsField() {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		UniqueConstraintMultipleNavigable inv = UniqueConstraintMultipleNavigable.newInstance();
		bean.setInvAggAssociation(inv);
		bean.nullInvAggAssociation();
		assertNull(bean.getInvAggAssociation());
	}
}
