package modules.test;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.util.test.TestUtil;

import modules.test.domain.UniqueConstraintNonNullable;
import modules.test.domain.UniqueConstraintPersistent;

/**
 * Test unique constraint checking is only applied with persistent-by-reachability.
 * ie A transient reference to a persistable document wont be checked unless the bean is already persisted.
 */
class UniqueConstraintPersistentTest extends AbstractSkyveTest {

	private UniqueConstraintNonNullable uc1;
	private UniqueConstraintNonNullable uc2;

	@BeforeEach
	void setup() throws Exception {
		uc1 = TestUtil.constructRandomInstance(u, m, ucnn, 0);
		uc2 = TestUtil.constructRandomInstance(u, m, ucnn, 0);
	}

	private void duplicate() {
		uc2.setBooleanFlag(uc1.getBooleanFlag());
		uc2.setEnum3(uc1.getEnum3());
		uc2.setText(uc1.getText());
	}
	
	// Single reference tests
	
	@Test
	void testNonPersistentReferenceToPersistedDuplicates() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
			ucp.setNonPersistent1(uc1);
			ucp.setNonPersistent2(uc2);
			ucp = p.save(ucp);
		});

		assertTrue(ucve.getMessages().size() > 0);
	}
	
	@Test
	void testNonPersistentReferenceToUnpersistedDuplicates() {
		duplicate();
		
		UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
		ucp.setNonPersistent1(uc1);
		ucp.setNonPersistent2(uc2);
		ucp = p.save(ucp);
	}

	@Test
	void testPersistentReferenceToPersistedDuplicates() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
			ucp.setPersistent1(uc1);
			ucp.setPersistent2(uc2);
			p.save(ucp);
		});

		assertTrue(ucve.getMessages().size() > 0);
	}
	
	@Test
	void testPersistentReferenceToUnpersistedDuplicates() {
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
			ucp.setPersistent1(uc1);
			ucp.setPersistent2(uc2);
			ucp = p.save(ucp);
		});

		assertTrue(ucve.getMessages().size() > 0);
	}

	@Test
	void testMixedReferenceToPersistedDuplicates() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
			ucp.setPersistent1(uc1);
			ucp.setNonPersistent2(uc2);
			p.save(ucp);
		});

		assertTrue(ucve.getMessages().size() > 0);
	}
	
	@Test
	void testMixedReferenceToUnpersistedDuplicates() {
		duplicate();
		
		UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
		ucp.setPersistent1(uc1);
		ucp.setNonPersistent2(uc2);
		ucp = p.save(ucp);
	}

	@Test
	void testNonPersistentReferenceToPersistedUniques() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		
		UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
		ucp.setNonPersistent1(uc1);
		ucp.setNonPersistent2(uc2);
		p.save(ucp);
	}
	
	@Test
	void testNonPersistentReferenceToUnpersistedUniques() {
		UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
		ucp.setNonPersistent1(uc1);
		ucp.setNonPersistent2(uc2);
		p.save(ucp);
	}

	@Test
	void testPersistentReferenceToPersistedUniques() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		
		UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
		ucp.setPersistent1(uc1);
		ucp.setPersistent2(uc2);
		p.save(ucp);
	}
	
	@Test
	void testPersistentReferenceToUnpersistedUniques() {
		UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
		ucp.setPersistent1(uc1);
		ucp.setPersistent2(uc2);
		p.save(ucp);
	}

	@Test
	void testMixedReferenceToPersistedUniques() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		
		UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
		ucp.setPersistent1(uc1);
		ucp.setNonPersistent2(uc2);
		p.save(ucp);
	}
	
	@Test
	void testMixedReferenceToUnpersistedUniques() {
		UniqueConstraintPersistent ucp = UniqueConstraintPersistent.newInstance();
		ucp.setPersistent1(uc1);
		ucp.setNonPersistent2(uc2);
		p.save(ucp);
	}

	// Non-persistent indirect multiple reference tests
	
	@Test
	void testNonPersistentToNonPersistentReferenceToPersistedDuplicates() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
			innerUcp.setNonPersistent1(uc1);
			innerUcp.setNonPersistent2(uc2);
			UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
			outerUcp.setNonPersistentReference(innerUcp);
			p.save(outerUcp);
		});

		assertTrue(ucve.getMessages().size() > 0);
	}

	@Test
	void testNonPersistentToNonPersistentReferenceToUnpersistedDuplicates() {
		duplicate();
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setNonPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setNonPersistentReference(innerUcp);
		p.save(outerUcp);
	}

	@Test
	void testNonPersistentToPersistentReferenceToPersistedDuplicates() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
			innerUcp.setPersistent1(uc1);
			innerUcp.setPersistent2(uc2);
			UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
			outerUcp.setNonPersistentReference(innerUcp);
			p.save(outerUcp);
		});

		assertTrue(ucve.getMessages().size() > 0);
	}
	
	@Test
	void testNonPersistentToPersistentReferenceToUnpersistedDuplicates() {
		duplicate();
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setNonPersistentReference(innerUcp);
		p.save(outerUcp);
	}

	@Test
	void testNonPersistentToMixedReferenceToPersistedDuplicates() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
			innerUcp.setPersistent1(uc1);
			innerUcp.setNonPersistent2(uc2);
			UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
			outerUcp.setNonPersistentReference(innerUcp);
			p.save(outerUcp);
		});

		assertTrue(ucve.getMessages().size() > 0);
	}

	@Test
	void testNonPersistentToMixedReferenceToUnpersistedDuplicates() {
		duplicate();
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setNonPersistentReference(innerUcp);
		p.save(outerUcp);
	}

	@Test
	void testNonPersistentToNonPersistentReferenceToPersistedUniques() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setNonPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setNonPersistentReference(innerUcp);
		p.save(outerUcp);
	}
	
	@Test
	void testNonPersistentToNonPersistentReferenceToUnpersistedUniques() {
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setNonPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setNonPersistentReference(innerUcp);
		p.save(outerUcp);
	}

	@Test
	void testNonPersistentToPersistentReferenceToPersistedUniques() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setNonPersistentReference(innerUcp);
		p.save(outerUcp);
	}
	
	@Test
	void testNonPersistentToPersistentReferenceToUnpersistedUniques() {
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setNonPersistentReference(innerUcp);
		p.save(outerUcp);
	}

	@Test
	void testNonPersistentToMixedReferenceToPersistedUniques() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setNonPersistentReference(innerUcp);
		p.save(outerUcp);
	}
	
	@Test
	void testNonPersistentToMixedReferenceToUnpersistedUniques() {
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setNonPersistentReference(innerUcp);
		p.save(outerUcp);
	}
	
	// Persistent indirect multiple reference tests
	
	@Test
	void testPersistentToNonPersistentReferenceToPersistedDuplicates() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
			innerUcp.setNonPersistent1(uc1);
			innerUcp.setNonPersistent2(uc2);
			UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
			outerUcp.setPersistentReference(innerUcp);
			p.save(outerUcp);
		});

		assertTrue(ucve.getMessages().size() > 0);
	}

	@Test
	void testPersistentToNonPersistentReferenceToUnpersistedDuplicates() {
		duplicate();
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setNonPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setPersistentReference(innerUcp);
		p.save(outerUcp);
	}

	@Test
	void testPersistentToPersistentReferenceToPersistedDuplicates() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
			innerUcp.setPersistent1(uc1);
			innerUcp.setPersistent2(uc2);
			UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
			outerUcp.setPersistentReference(innerUcp);
			p.save(outerUcp);
		});

		assertTrue(ucve.getMessages().size() > 0);
	}
	
	@Test
	void testPersistentToPersistentReferenceToUnpersistedDuplicates() {
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
			innerUcp.setPersistent1(uc1);
			innerUcp.setPersistent2(uc2);
			UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
			outerUcp.setPersistentReference(innerUcp);
			p.save(outerUcp);
		});
		
		assertTrue(ucve.getMessages().size() > 0);
	}

	@Test
	void testPersistentToMixedReferenceToPersistedDuplicates() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		duplicate();
		
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
			innerUcp.setPersistent1(uc1);
			innerUcp.setNonPersistent2(uc2);
			UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
			outerUcp.setPersistentReference(innerUcp);
			p.save(outerUcp);
		});

		assertTrue(ucve.getMessages().size() > 0);
	}

	@Test
	void testPersistentToMixedReferenceToUnpersistedDuplicates() {
		duplicate();
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setPersistentReference(innerUcp);
		p.save(outerUcp);
	}

	@Test
	void testPersistentToNonPersistentReferenceToPersistedUniques() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setNonPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setPersistentReference(innerUcp);
		p.save(outerUcp);
	}
	
	@Test
	void testPersistentToNonPersistentReferenceToUnpersistedUniques() {
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setNonPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setPersistentReference(innerUcp);
		p.save(outerUcp);
	}

	@Test
	void testPersistentToPersistentReferenceToPersistedUniques() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setPersistentReference(innerUcp);
		p.save(outerUcp);
	}
	
	@Test
	void testPersistentToPersistentReferenceToUnpersistedUniques() {
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setPersistentReference(innerUcp);
		p.save(outerUcp);
	}

	@Test
	void testPersistentToMixedReferenceToPersistedUniques() {
		uc1 = p.save(uc1);
		uc2 = p.save(uc2);
		
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setPersistentReference(innerUcp);
		p.save(outerUcp);
	}
	
	@Test
	void testPersistentToMixedReferenceToUnpersistedUniques() {
		UniqueConstraintPersistent innerUcp = UniqueConstraintPersistent.newInstance();
		innerUcp.setPersistent1(uc1);
		innerUcp.setNonPersistent2(uc2);
		UniqueConstraintPersistent outerUcp = UniqueConstraintPersistent.newInstance();
		outerUcp.setPersistentReference(innerUcp);
		p.save(outerUcp);
	}
}
