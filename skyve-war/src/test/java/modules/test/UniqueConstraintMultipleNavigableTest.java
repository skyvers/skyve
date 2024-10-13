package modules.test;

import org.junit.jupiter.api.Test;
import org.skyve.util.test.TestUtil;

import modules.test.domain.UniqueConstraintMultipleNavigable;

public class UniqueConstraintMultipleNavigableTest extends AbstractSkyveTest {
	/**
	 * Test bean1 references itself with an inverse
	 */
	@Test
	public void testSaveCyclic() throws Exception {
		UniqueConstraintMultipleNavigable bean = TestUtil.constructRandomInstance(u, m, ucmn, 0);
		bean.setAggAssociation(bean);

		p.save(bean);
	}
	
	/**
	 * Test bean1 references bean2 and bean2 is sent to be save simultaneously
	 */
	@Test
	public void testSaveVarArgCyclic() throws Exception {
		UniqueConstraintMultipleNavigable bean1 = TestUtil.constructRandomInstance(u, m, ucmn, 0);
		UniqueConstraintMultipleNavigable bean2 = TestUtil.constructRandomInstance(u, m, ucmn, 0);
		bean1.setAggAssociation(bean2);

		p.save(bean1, bean2);
	}
}
