package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.domain.messages.UniqueConstraintViolationException;

import modules.test.domain.UniqueConstraintOptimisation;

public class UniqueConstraintOptimisationTest extends AbstractSkyveTest {
	/**
	 * Test composite with a null (uc1uc2c) is not tested,
	 * single with null (uc3c) is not tested,
	 * relation with a null (pac) is not tested, and
	 * implicit (flagc) with a null is not tested.
	 * @throws Exception
	 */
	@Test
	public void testNullNotTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc1("1");
		bean1.setUc2(null);
		bean1 = p.save(bean1);
		
		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc1("1");
		bean2.setUc2(null);
		bean2 = p.save(bean2);
	}

	/**
	 * Test that a non-persistent relation with an unpersisted bean is ignored.
	 */
	@Test
	public void testNonPersistentUnpersistedRelationNotTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc1("1");
		bean1.setUc2("1");

		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc1("1");
		bean2.setUc2("1");
		
		bean1.setNonPersistentAssociation(bean2);

		p.save(bean1);
	}
	
	/**
	 * Test that a non-persistent relation with a persisted bean is tested.
	 */
	@Test(expected = UniqueConstraintViolationException.class)
	public void testNonPersistentPersistedRelationTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc1("1");
		bean1.setUc2("1");

		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc1("1");
		bean2.setUc2("1");
		bean2 = p.save(bean2);
		
		bean1.setNonPersistentAssociation(bean2);
		p.save(bean1);
	}
	
	@Test(expected = UniqueConstraintViolationException.class)
	public void testPersistentPersistedRelationTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc1("1");
		bean1.setUc2("1");
		bean1.setPersistentAssociation(bean1);
		bean1 = p.save(bean1);
		
		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc1("2");
		bean2.setUc2("2");
		bean2.setPersistentAssociation(bean1);
		bean2 = p.save(bean2);
	}

	// UC1/UC2 violated
	@Test(expected = UniqueConstraintViolationException.class)
	public void testPersistentNonPersistedRelationTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc1("1");
		bean1.setUc2("1");
		bean1 = p.save(bean1);
		
		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc1("1");
		bean2.setUc2("1");

		UniqueConstraintOptimisation bean3 = ucno.newInstance(u);
		bean3.setUc1("3");
		bean3.setUc2("3");
		bean3.setPersistentAssociation(bean2);
		bean3 = p.save(bean3);
	}

	// pa not violated
	@Test
	public void testPersistentNonPersistedRelationNotTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc1("1");
		bean1.setUc2("1");
		
		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc1("2");
		bean2.setUc2("2");
		bean1.setPersistentAssociation(bean2);
		
		UniqueConstraintOptimisation bean3 = ucno.newInstance(u);
		bean3.setUc1("3");
		bean3.setUc2("3");
		bean3.setPersistentAssociation(bean2);
		bean3 = p.save(bean3);
	}
	
	// flag violated on insert
	@Test(expected = UniqueConstraintViolationException.class)
	public void testImplicitAttributeInsertTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc1("1");
		bean1.setUc2("1");
		bean1.setBizFlagComment("SAME");
		bean1 = p.save(bean1);
		
		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc1("2");
		bean2.setUc2("2");
		bean2.setBizFlagComment("SAME");
		bean2 = p.save(bean2);
	}

	// flag violated on update - always tested because its an implicit attribute
	@Test(expected = UniqueConstraintViolationException.class)
	public void testImplicitAttributeTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc1("1");
		bean1.setUc2("1");
		bean1.setBizFlagComment("SAME");
		bean1 = p.save(bean1);
		
		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc1("2");
		bean2.setUc2("2");
		bean2.setBizFlagComment("DIFFERENT");
		bean2 = p.save(bean2);
		
		bean2.setBizFlagComment("SAME");
		bean2 = p.save(bean2);
	}
	
	// uc1/uc2 violated on update - when dirty
	@Test(expected = UniqueConstraintViolationException.class)
	public void testDirtyUpdateTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc1("1");
		bean1.setUc2("1");
		bean1 = p.save(bean1);
		
		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc1("2");
		bean2.setUc2("2");
		bean2 = p.save(bean2);
		
		bean2.setUc1("1");
		bean2.setUc2("1");
		bean2 = p.save(bean2);
	}

	// uc1/uc2 not violated on update - when made clean
	@Test
	public void testDirtyUpdateNotTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc1("1");
		bean1.setUc2("1");
		bean1 = p.save(bean1);
		
		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc1("2");
		bean2.setUc2("2");
		bean2 = p.save(bean2);
		
		bean2.setUc1("1");
		bean2.setUc2("1");
		// clear the original values and unique constraint checks wont occur
		bean2.originalValues().clear();
		bean2 = p.save(bean2);
	}

	// uc3 violated on update - even though not dirty, coz track changes is false
	@Test(expected = UniqueConstraintViolationException.class)
	public void testUntrackedUpdateTested() throws Exception {
		UniqueConstraintOptimisation bean1 = ucno.newInstance(u);
		bean1.setUc3("1");
		bean1 = p.save(bean1);
		
		UniqueConstraintOptimisation bean2 = ucno.newInstance(u);
		bean2.setUc3("2");
		bean2 = p.save(bean2);
		
		bean2.setUc3("1");
		Assert.assertEquals(0, bean2.originalValues().size());
		bean2 = p.save(bean2);
	}
}
