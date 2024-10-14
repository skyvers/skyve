package modules.test;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.util.test.TestUtil;

import modules.test.domain.AllAttributesPersistent;

public class H2UniqueConstraintTest extends AbstractSkyveTest {
	/**
	 * Test null uniqueness
	 */
	@Test
	public void testNullUniqueness() throws Exception {
		AllAttributesPersistent aap1 = TestUtil.constructRandomInstance(u, m, aapd, 0);
		aap1.setComposedAssociation(null);
		aap1 = p.save(aap1);
		
		AllAttributesPersistent aap2 = TestUtil.constructRandomInstance(u, m, aapd, 0);
		aap2.setComposedAssociation(null);
		p.save(aap2);
	}

	/**
	 * Test non-null uniqueness
	 */
	@Test
	public void testNonNullUniqueness() throws Exception {
		Assert.assertThrows(DomainException.class, () -> {
			AllAttributesPersistent aap1 = TestUtil.constructRandomInstance(u, m, aapd, 0);
			aap1.setComposedAssociation(aap1);
			aap1 = p.save(aap1);
			
			AllAttributesPersistent aap2 = TestUtil.constructRandomInstance(u, m, aapd, 0);
			aap2.setComposedAssociation(aap1);
			p.save(aap2);
		});
	}
}
