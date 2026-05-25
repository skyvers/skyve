package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class UniqueConstraintMultipleNavigableDomainTest extends AbstractH2Test {

	@Test
	void bizModuleIsTest() throws Exception {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		assertEquals("test", bean.getBizModule());
	}

	@Test
	void bizDocumentIsUniqueConstraintMultipleNavigable() throws Exception {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		assertEquals("UniqueConstraintMultipleNavigable", bean.getBizDocument());
	}

	@Test
	void getBizKeyNotNull() throws Exception {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void uniqueNameSetAndGet() throws Exception {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		bean.setUniqueName("testName");
		assertEquals("testName", bean.getUniqueName());
	}

	@Test
	void aggAssociationNullByDefault() throws Exception {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		assertNull(bean.getAggAssociation());
	}

	@Test
	void aggAssociationSetAndGet() throws Exception {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		UniqueConstraintMultipleNavigable assoc = UniqueConstraintMultipleNavigable.newInstance();
		bean.setAggAssociation(assoc);
		assertEquals(assoc, bean.getAggAssociation());
	}

	@Test
	void settingAggAssociationSetsInverseOnAssoc() throws Exception {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		UniqueConstraintMultipleNavigable assoc = UniqueConstraintMultipleNavigable.newInstance();
		bean.setAggAssociation(assoc);
		assertEquals(bean, assoc.getInvAggAssociation());
	}

	@Test
	void nullAggAssociationClearsField() throws Exception {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		UniqueConstraintMultipleNavigable assoc = UniqueConstraintMultipleNavigable.newInstance();
		bean.setAggAssociation(assoc);
		bean.nullAggAssociation();
		assertNull(bean.getAggAssociation());
	}

	@Test
	void invAggAssociationNullByDefault() throws Exception {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		assertNull(bean.getInvAggAssociation());
	}

	@Test
	void nullInvAggAssociationClearsField() throws Exception {
		UniqueConstraintMultipleNavigable bean = UniqueConstraintMultipleNavigable.newInstance();
		UniqueConstraintMultipleNavigable inv = UniqueConstraintMultipleNavigable.newInstance();
		bean.setInvAggAssociation(inv);
		bean.nullInvAggAssociation();
		assertNull(bean.getInvAggAssociation());
	}
}
