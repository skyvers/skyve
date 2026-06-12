package modules.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Objects;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.ExportedReferenceVisitor.Dereferencer;
import org.skyve.util.Util;

import modules.test.domain.Hierarchical;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

class ExportedReferenceVisitorTests extends AbstractSkyveTest {

	@Test
	void testDereferencerOnHierarchical() throws Exception {
		Hierarchical root = Util.constructRandomInstance(u, m, hd, 1);
		root = p.save(root);
		Hierarchical child = Util.constructRandomInstance(u, m, hd, 1);
		child.setBizParentId(root.getBizId());
		child = p.save(child);

		p.evictCached(child);

		assertEquals(2, Objects.requireNonNull(p.newSQL("select count(1) from TEST_Hierarchical").scalarResult(Number.class)).intValue());

		new Dereferencer().visit(hd, root);
		child = Objects.requireNonNull(p.retrieve(hd, child.getBizId()));
		assertNull(child.getParent());
		p.delete(root);

		assertEquals(1, Objects.requireNonNull(p.newSQL("select count(1) from TEST_Hierarchical").scalarResult(Number.class)).intValue());
	}

	@Test
	void testDereferencerOnMappedExtensionSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 3);

		// weave a web with the aggregated association
		test.setComposedAssociation(test.getAggregatedAssociation());
		test.getComposedCollection().set(0, test.getAggregatedAssociation());
		test.getAggregatedCollection().set(0, test.getAggregatedAssociation());

		test = p.save(test);

		new Dereferencer().visit(test.getAggregatedAssociation());

		p.evictAllCached();

		test = p.retrieve(messd, test.getBizId());
		assertNotNull(test);
		assertNull(test.getAggregatedAssociation());
		assertNull(test.getComposedAssociation());
		assertEquals(1, test.getAggregatedCollection().size());
		assertEquals(1, test.getComposedCollection().size());
	}

	@Test
	void testDereferencerOnMappedSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 3);

		// weave a web with the aggregated association
		test.setComposedAssociation(test.getAggregatedAssociation());
		test.getComposedCollection().set(0, test.getAggregatedAssociation());
		test.getAggregatedCollection().set(0, test.getAggregatedAssociation());

		test = p.save(test);

		new Dereferencer().visit(test.getAggregatedAssociation());

		p.evictAllCached();

		test = p.retrieve(msssd, test.getBizId());
		assertNotNull(test);
		assertNull(test.getAggregatedAssociation());
		assertNull(test.getComposedAssociation());
		assertEquals(1, test.getAggregatedCollection().size());
		assertEquals(1, test.getComposedCollection().size());
	}
}
