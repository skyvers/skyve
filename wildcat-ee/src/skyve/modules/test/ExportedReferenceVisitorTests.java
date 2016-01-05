package modules.test;

import modules.test.domain.Hierarchical;
import modules.test.domain.MappedExtension;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.util.Util;
import org.skyve.wildcat.util.ExportedReferenceVisitor.Dereferencer;

public class ExportedReferenceVisitorTests extends AbstractH2Test {
	@Test
	public void testDereferencerOnHierarchical() throws Exception {
		Hierarchical root = Util.constructRandomInstance(u, m, hd, 1);
		root = p.save(root);
		Hierarchical child = Util.constructRandomInstance(u, m, hd, 1);
		child.setBizParentId(root.getBizId());
		child = p.save(child);
		
		p.evictCached(child);
		
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_Hierarchical").scalarResult(Number.class).intValue()); 

		new Dereferencer().visit(hd, root);
		child = p.retrieve(hd, child.getBizId(), false);
		Assert.assertNull(child.getParent());
		p.delete(root);
		
		Assert.assertEquals(1, p.newSQL("select count(1) from TEST_Hierarchical").scalarResult(Number.class).intValue()); 
	}
	
	@Test
	public void testDereferencerOnMappedExtension() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 3);
		
		// weave a web with the aggregated association
		test.setComposedAssociation(test.getAggregatedAssociation());
		test.getComposedCollection().set(0, test.getAggregatedAssociation());
		test.getAggregatedCollection().set(0, test.getAggregatedAssociation());
		
		test = p.save(test);
		
		new Dereferencer().visit(test.getAggregatedAssociation());
		
		p.evictAllCached();
		
		test = p.retrieve(med, test.getBizId(), false);
		Assert.assertNull(test.getAggregatedAssociation());
		Assert.assertNull(test.getComposedAssociation());
		Assert.assertEquals(test.getAggregatedCollection().size(), 1);
		Assert.assertEquals(test.getComposedCollection().size(), 1);
	}
}
