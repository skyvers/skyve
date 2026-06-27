package modules.test;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.util.test.TestUtil;

import modules.test.domain.UniqueConstraintMultipleNavigable;

@SuppressWarnings({ "java:S5778", "java:S1130", "java:S1854" })
class UniqueConstraintMultipleNavigableTest extends AbstractSkyveTest {
	/**
	 * Test bean1 references itself with an inverse
	 */
	@Test
	void testSaveCyclic() throws Exception {
		UniqueConstraintMultipleNavigable bean = Assertions.assertDoesNotThrow(() -> TestUtil.constructRandomInstance(u, m, ucmn, 0));
		bean.setAggAssociation(bean);

		p.save(bean);
	}
	
	/**
	 * Test bean1 references bean2 and bean2 is sent to be save simultaneously
	 */
	@Test
	void testSaveVarArgCyclic() throws Exception {
		UniqueConstraintMultipleNavigable bean1 = Assertions.assertDoesNotThrow(() -> TestUtil.constructRandomInstance(u, m, ucmn, 0));
		UniqueConstraintMultipleNavigable bean2 = TestUtil.constructRandomInstance(u, m, ucmn, 0);
		bean1.setAggAssociation(bean2);

		p.save(bean1, bean2);
	}
}
