package modules.test;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.util.Util;

import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtensionUniqueJoinedStrategy;
import modules.test.domain.MappedExtensionUniqueSingleStrategy;
import modules.test.domain.MappedSubclassedUniqueJoinedStrategy;
import modules.test.domain.MappedSubclassedUniqueSingleStrategy;

public class UniqueConstraintHierarchyTest extends AbstractSkyveTest {
	/**
	 * Insert the base document and then insert a duplicate extension document in the same transaction.
	 */
	@Test
	public void testSingleStrategyBaseInSameTransaction() throws Exception {
		MappedExtensionUniqueSingleStrategy test1 = Util.constructRandomInstance(u, m, meussd, 1);
		MappedSubclassedUniqueSingleStrategy test2 = Util.constructRandomInstance(u, m, msussd, 1);
		inSameTransaction(test1, test2, true);
	}

	/**
	 * Insert the base document and then insert a duplicate extension document in a different transaction.
	 */
	@Test
	public void testSingleStrategyBaseInAnotherTransaction() throws Exception {
		MappedExtensionUniqueSingleStrategy test1 = Util.constructRandomInstance(u, m, meussd, 1);
		MappedSubclassedUniqueSingleStrategy test2 = Util.constructRandomInstance(u, m, msussd, 1);
		inAnotherTransaction(test1, test2, true);
	}

	/**
	 * Insert the base document, commit and then insert a duplicate extension document.
	 */
	@Test
	public void testSingleStrategyBaseWithCommit() throws Exception {
		MappedExtensionUniqueSingleStrategy test1 = Util.constructRandomInstance(u, m, meussd, 1);
		MappedSubclassedUniqueSingleStrategy test2 = Util.constructRandomInstance(u, m, msussd, 1);
		withCommit(test1, test2, true);
	}

	/**
	 * Insert the base document and then insert a duplicate extension document in the same transaction.
	 */
	@Test
	public void testJoinedStrategyBaseInSameTransaction() throws Exception {
		MappedExtensionUniqueJoinedStrategy test1 = Util.constructRandomInstance(u, m, meujsd, 1);
		MappedSubclassedUniqueJoinedStrategy test2 = Util.constructRandomInstance(u, m, msujsd, 1);
		inSameTransaction(test1, test2, true);
	}

	/**
	 * Insert the base document and then insert a duplicate extension document in a different transaction.
	 */
	@Test
	public void testJoinedStrategyBaseInAnotherTransaction() throws Exception {
		MappedExtensionUniqueJoinedStrategy test1 = Util.constructRandomInstance(u, m, meujsd, 1);
		MappedSubclassedUniqueJoinedStrategy test2 = Util.constructRandomInstance(u, m, msujsd, 1);
		inAnotherTransaction(test1, test2, true);
	}

	/**
	 * Insert the base document, commit and then insert a duplicate extension document.
	 */
	@Test
	public void testJoinedStrategyBaseWithCommit() throws Exception {
		MappedExtensionUniqueJoinedStrategy test1 = Util.constructRandomInstance(u, m, meujsd, 1);
		MappedSubclassedUniqueJoinedStrategy test2 = Util.constructRandomInstance(u, m, msujsd, 1);
		withCommit(test1, test2, true);
	}

	/**
	 * Insert duplicate id extension documents in the same transaction.
	 */
	@Test
	public void testSingleStrategyDerivedInSameTransaction() throws Exception {
		MappedSubclassedUniqueSingleStrategy test1 = Util.constructRandomInstance(u, m, msussd, 1);
		MappedSubclassedUniqueSingleStrategy test2 = Util.constructRandomInstance(u, m, msussd, 1);
		inSameTransaction(test1, test2, false);
	}

	/**
	 * Insert duplicate id extension documents in a different transaction.
	 */
	@Test
	public void testSingleStrategyDerivedInAnotherTransaction() throws Exception {
		MappedSubclassedUniqueSingleStrategy test1 = Util.constructRandomInstance(u, m, msussd, 1);
		MappedSubclassedUniqueSingleStrategy test2 = Util.constructRandomInstance(u, m, msussd, 1);
		inAnotherTransaction(test1, test2, false);
	}

	/**
	 * Insert duplicate id extension documents with commit in between.
	 */
	@Test
	public void testSingleStrategyDerivedWithCommit() throws Exception {
		MappedSubclassedUniqueSingleStrategy test1 = Util.constructRandomInstance(u, m, msussd, 1);
		MappedSubclassedUniqueSingleStrategy test2 = Util.constructRandomInstance(u, m, msussd, 1);
		withCommit(test1, test2, false);
	}

	/**
	 * Insert duplicate id extension documents in the same transaction.
	 */
	@Test
	public void testJoinedStrategyDerivedInSameTransaction() throws Exception {
		MappedSubclassedUniqueJoinedStrategy test1 = Util.constructRandomInstance(u, m, msujsd, 1);
		MappedSubclassedUniqueJoinedStrategy test2 = Util.constructRandomInstance(u, m, msujsd, 1);
		inSameTransaction(test1, test2, false);
	}

	/**
	 * Insert duplicate id extension documents in a different transaction.
	 */
	@Test
	public void testJoinedStrategyDerivedInAnotherTransaction() throws Exception {
		MappedSubclassedUniqueJoinedStrategy test1 = Util.constructRandomInstance(u, m, msujsd, 1);
		MappedSubclassedUniqueJoinedStrategy test2 = Util.constructRandomInstance(u, m, msujsd, 1);
		inAnotherTransaction(test1, test2, false);
	}

	/**
	 * Insert duplicate id extension documents with a commit in between.
	 */
	@Test
	public void testJoinedStrategyDerivedWithCommit() throws Exception {
		MappedSubclassedUniqueJoinedStrategy test1 = Util.constructRandomInstance(u, m, msujsd, 1);
		MappedSubclassedUniqueJoinedStrategy test2 = Util.constructRandomInstance(u, m, msujsd, 1);
		withCommit(test1, test2, false);
	}

	private static void duplicate(MappedBase bean, boolean text) {
		if (text) {
			bean.setText("text");
			bean.setId(UUID.randomUUID().toString());
		}
		else {
			bean.setId("id");
			bean.setText(UUID.randomUUID().toString());
		}
	}
	
	private void withCommit(MappedBase test1, MappedBase test2, boolean text) {
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			duplicate(test1, text);
			p.save(test1);
			p.commit(false);
			p.begin();
			duplicate(test2, text);
			p.save(test2);
		});
		
		assertTrue(ucve.getMessages().size() > 0);
	}
	
	private void inAnotherTransaction(MappedBase test1, MappedBase test2, boolean text) {
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			duplicate(test1, text);
			p.save(test1);
			AbstractPersistence p1 = AbstractPersistence.newInstance();
			p1.setUser(p.getUser());
			p1.begin();
			duplicate(test2, text);
			p1.save(test2);
		});
		
		assertTrue(ucve.getMessages().size() > 0);
	}
	
	private void inSameTransaction(MappedBase test1, MappedBase test2, boolean text) {
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
			duplicate(test1, text);
			p.save(test1);
			duplicate(test2, text);
			p.save(test2);
		});
		
		assertTrue(ucve.getMessages().size() > 0);
	}
	
	/**
	 * Insert and then delete the same bean to ensure the unique state (hashes and ADM_Uniqueness rows) has been removed.
	 */
	@Test
	public void testNoUniquenessStateAfterDelete() throws Exception {
		// Insert a bean
		MappedExtensionUniqueJoinedStrategy test1 = Util.constructRandomInstance(u, m, meujsd, 1);
		test1.setText("text");
		test1 = p.save(test1);
		// Delete it straight away
		p.delete(test1);
		// Should be able to insert a new bean with the same unique text value
		MappedSubclassedUniqueJoinedStrategy test2 = Util.constructRandomInstance(u, m, msujsd, 1);
		test2.setText("text");
		p.save(test2);
	}
}
