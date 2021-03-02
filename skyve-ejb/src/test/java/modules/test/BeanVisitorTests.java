package modules.test;

import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.util.BeanVisitor;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.admin.domain.ImportExport;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.InverseOneToOnePersistent;

public class BeanVisitorTests extends AbstractSkyveTest {
	@Test
	public void testStandard() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);

		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		expectedBindings.add(AllAttributesPersistent.aggregatedAssociationPropertyName);
		expectedBindings.add(Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 0));
		expectedBindings.add(Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 1));
		expectedBindings.add(AllAttributesPersistent.composedAssociationPropertyName);
		expectedBindings.add(AllAttributesPersistent.embeddedAssociationPropertyName);

		final Set<String> actualBindings = new TreeSet<>();

		new BeanVisitor(false, false, false) {
			@Override
			protected boolean accept(String binding,
					Document document,
					Document owningDocument,
					Relation owningRelation,
					Bean bean) throws Exception {
				System.out.println("B = " + binding);
				actualBindings.add(binding);
				return true;
			}

		}.visit(aapd, test, c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}

	@Test
	public void testNull() throws Exception {
		Module admin = c.getModule(ImportExport.MODULE_NAME);
		Document ieDoc = admin.getDocument(c, ImportExport.DOCUMENT_NAME);
		ImportExport test = Util.constructRandomInstance(u, admin, ieDoc, 2);
		
		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// importExportColumns[0]
		String expectedBinding = Binder.createIndexedBinding(ImportExport.importExportColumnsPropertyName, 0);
		expectedBindings.add(expectedBinding);
		// importExportColumns[0].parent
		expectedBinding = Binder.createCompoundBinding(expectedBinding, ChildBean.PARENT_NAME);
		expectedBindings.add(expectedBinding);
		// importExportColumns[0].parent.importExportColumns
		expectedBinding = Binder.createCompoundBinding(expectedBinding, ImportExport.importExportColumnsPropertyName);
		expectedBindings.add(expectedBinding);
		// importExportColumns[0].parent.importExportColumns.parent
		expectedBinding = Binder.createCompoundBinding(expectedBinding, ChildBean.PARENT_NAME);
		expectedBindings.add(expectedBinding);
		// importExportColumns[1]
		expectedBinding = Binder.createIndexedBinding(ImportExport.importExportColumnsPropertyName, 1);
		expectedBindings.add(expectedBinding);
		// importExportColumns[1].parent
		expectedBinding = Binder.createCompoundBinding(expectedBinding, ChildBean.PARENT_NAME);
		expectedBindings.add(expectedBinding);
		// importExportColumns[1].parent.importExportColumns
		expectedBinding = Binder.createCompoundBinding(expectedBinding, ImportExport.importExportColumnsPropertyName);
		expectedBindings.add(expectedBinding);
		// importExportColumns[1].parent.importExportColumns.parent
		expectedBinding = Binder.createCompoundBinding(expectedBinding, ChildBean.PARENT_NAME);
		expectedBindings.add(expectedBinding);
		

		final Set<String> actualBindings = new TreeSet<>();

		new BeanVisitor(true, false, false) {
			@Override
			protected boolean accept(String binding,
					Document document,
					Document owningDocument,
					Relation owningRelation,
					Bean bean) throws Exception {
				System.out.println("B = " + binding);
				actualBindings.add(binding);
				return true;
			}

		}.visit(ieDoc, test, c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}

	@Test
	public void testNotNull() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().get(0).setAggregatedAssociation(null);
		test.getAggregatedCollection().remove(1);

		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// allAttributesPersistents[0]
		expectedBindings.add(Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 0));
		// composedAssociation
		expectedBindings.add(AllAttributesPersistent.composedAssociationPropertyName);
		// embeddedAssociation
		expectedBindings.add(AllAttributesPersistent.embeddedAssociationPropertyName);

		final Set<String> actualBindings = new TreeSet<>();

		new BeanVisitor(false, false, false) {
			@Override
			protected boolean accept(String binding,
					Document document,
					Document owningDocument,
					Relation owningRelation,
					Bean bean) throws Exception {
				System.out.println("B = " + binding);
				actualBindings.add(binding);
				return true;
			}

		}.visit(aapd, test, c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}

	@Test
	public void testManyToOneInverses() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		
		// Load inverses
		test = p.save(test);
		p.evictAllCached();
		test = p.retrieve(aapd, test.getBizId());
		
		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// inverseAggregatedAssociation[0]
		String expectedBinding = Binder.createIndexedBinding(AllAttributesPersistent.inverseAggregatedAssociationPropertyName, 0);
		expectedBindings.add(expectedBinding);
		// inverseAggregatedAssociation[0].composedAssociation
		expectedBindings.add(Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.composedAssociationPropertyName));
		// inverseAggregatedAssociation[0].embeddedAssociation
		expectedBindings.add(Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.embeddedAssociationPropertyName));
		expectedBinding = Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.embeddedAssociationPropertyName, AllAttributesPersistent.aggregatedCollectionPropertyName);
		// inverseAggregatedAssociation[0].embeddedAssociation.aggregatedCollection[0]
		expectedBindings.add(Binder.createIndexedBinding(expectedBinding, 0));
		// inverseAggregatedAssociation[0].embeddedAssociation.aggregatedCollection[1]
		expectedBindings.add(Binder.createIndexedBinding(expectedBinding, 1));

		final Set<String> actualBindings = new TreeSet<>();

		new BeanVisitor(false, true, false) {
			@Override
			protected boolean accept(String binding,
					Document document,
					Document owningDocument,
					Relation owningRelation,
					Bean bean) throws Exception {
				System.out.println("B = " + binding);
				actualBindings.add(binding);
				return true;
			}

		}.visit(aapd, test.getAggregatedAssociation(), c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}

	@Test
	public void testOneToOneInverses() throws Exception {
		InverseOneToOnePersistent test = Util.constructRandomInstance(u, m, io2opd, 2);
		// Load inverses
		test = p.save(test);
		p.evictAllCached();
		test = p.retrieve(io2opd, test.getBizId());

		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// inverse
		String expectedBinding = InverseOneToOnePersistent.invAggAssociationPropertyName;
		expectedBindings.add(expectedBinding);

		final Set<String> actualBindings = new TreeSet<>();

		new BeanVisitor(false, true, false) {
			@Override
			protected boolean accept(String binding,
					Document document,
					Document owningDocument,
					Relation owningRelation,
					Bean bean) throws Exception {
				System.out.println("B = " + binding);
				actualBindings.add(binding);
				return true;
			}

		}.visit(io2opd, test.getAggAssociation(), c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}

	@Test
	public void testScalar() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.setAggregatedAssociation(test);
		test.getAggregatedCollection().set(0, test);

		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// aggregatedCollection[1]
		expectedBindings.add(Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 1));
		// composedAssociation
		expectedBindings.add(AllAttributesPersistent.composedAssociationPropertyName);
		// embeddedAssociation
		expectedBindings.add(AllAttributesPersistent.embeddedAssociationPropertyName);
		
		final Set<String> actualBindings = new TreeSet<>();

		new BeanVisitor(false, false, false) {
			@Override
			protected boolean accept(String binding,
					Document document,
					Document owningDocument,
					Relation owningRelation,
					Bean bean) throws Exception {
				System.out.println("B = " + binding);
				actualBindings.add(binding);
				return true;
			}

		}.visit(aapd, test, c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}

	@Test
	public void testVector() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.getAggregatedCollection().set(0, test.getAggregatedAssociation());

		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// aggregatedAssociation
		expectedBindings.add(AllAttributesPersistent.aggregatedAssociationPropertyName);
		// composedAssociation
		expectedBindings.add(AllAttributesPersistent.composedAssociationPropertyName);
		// embeddedAssociation
		expectedBindings.add(AllAttributesPersistent.embeddedAssociationPropertyName);
		// embeddedAssociation.parent
		expectedBindings.add(Binder.createCompoundBinding(AllAttributesPersistent.embeddedAssociationPropertyName,
															ChildBean.PARENT_NAME));
		// embeddedAssociation.parent.aggregatedCollection[0]
		expectedBindings.add(Binder.createCompoundBinding(AllAttributesPersistent.embeddedAssociationPropertyName,
															ChildBean.PARENT_NAME,
															Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 0)));
		// embeddedAssociation.parentaggregatedCollection[1]
		expectedBindings.add(Binder.createCompoundBinding(AllAttributesPersistent.embeddedAssociationPropertyName,
															ChildBean.PARENT_NAME,
															Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 1)));

		final Set<String> actualBindings = new TreeSet<>();

		new BeanVisitor(false, false, true) {
			@Override
			protected boolean accept(String binding,
					Document document,
					Document owningDocument,
					Relation owningRelation,
					Bean bean) throws Exception {
				System.out.println("BV = " + binding);
				actualBindings.add(binding);
				return true;
			}

		}.visit(aapd, test, c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}
}
