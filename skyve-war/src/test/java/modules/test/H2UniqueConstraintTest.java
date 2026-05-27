package modules.test;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.domain.messages.DomainException;
import org.skyve.util.test.TestUtil;

import modules.test.domain.AllAttributesPersistent;

@SuppressWarnings({ "java:S1854", "java:S5778", "java:S1130" })
class H2UniqueConstraintTest extends AbstractSkyveTest {
	/**
	 * Test null uniqueness
	 */
	@Test
	void testNullUniqueness() throws Exception {
		AllAttributesPersistent aap1 = Assertions.assertDoesNotThrow(() -> TestUtil.constructRandomInstance(u, m, aapd, 0));
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
	void testNonNullUniqueness() throws Exception {
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
