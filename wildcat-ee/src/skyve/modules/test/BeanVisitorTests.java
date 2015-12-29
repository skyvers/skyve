package modules.test;

import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.wildcat.util.BeanVisitor;

import modules.test.domain.AllAttributesPersistent;

public class BeanVisitorTests extends AbstractH2Test {
	private Persistence p;
	private User u;
	private Customer c;
	private Module m;
	private Document d;

	@Before
	public void before() throws Exception {
		p = CORE.getPersistence();
		u = p.getUser();
		c = u.getCustomer();
		m = c.getModule(AllAttributesPersistent.MODULE_NAME);
		d = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
	}

	@Test
	public void testStandard() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 2);

		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		expectedBindings.add(AllAttributesPersistent.aggregatedAssociationPropertyName);
		expectedBindings.add(Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 0));
		expectedBindings.add(Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 1));
		
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
			
		}.visit(d, test, c);
		
		Assert.assertEquals(expectedBindings, actualBindings);
	}

	@Test
	public void testNull() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().get(0).setAggregatedAssociation(null);
		test.getAggregatedCollection().remove(1);

		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// aggregatedAssociation
		String expectedBinding = AllAttributesPersistent.aggregatedAssociationPropertyName;
		expectedBindings.add(expectedBinding);
		// aggregatedAssociation.aggregatedCollection
		expectedBinding = Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.aggregatedCollectionPropertyName);
		expectedBindings.add(expectedBinding);
		// aggregatedAssociation.aggregatedCollection.aggregatedAssociation
		expectedBindings.add(Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.aggregatedAssociationPropertyName));
		// aggregatedCollection[0]
		expectedBinding = Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 0);
		expectedBindings.add(expectedBinding);
		// aggregatedCollection[0].aggregatedAssociation
		expectedBinding = Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.aggregatedAssociationPropertyName);
		expectedBindings.add(expectedBinding);
		// aggregatedCollection[0].aggregatedAssociation.aggregatedCollection
		expectedBinding = Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.aggregatedCollectionPropertyName);
		expectedBindings.add(expectedBinding);
		// aggregatedCollection[0].aggregatedAssociation.aggregatedCollection.aggregatedAssociation
		expectedBinding = Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.aggregatedAssociationPropertyName);
		expectedBindings.add(expectedBinding);
		// aggregatedCollection[0].aggregatedCollection
		expectedBinding = Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 0);
		expectedBinding = Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.aggregatedCollectionPropertyName);
		expectedBindings.add(expectedBinding);
		// aggregatedCollection[0].aggregatedCollection.aggregatedAssociation
		expectedBinding = Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.aggregatedAssociationPropertyName);
		expectedBindings.add(expectedBinding);
		// aggregatedCollection[0].aggregatedCollection.aggregatedAssociation.aggregatedCollection
		expectedBinding = Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.aggregatedCollectionPropertyName);
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
			
		}.visit(d, test, c);
		
		Assert.assertEquals(expectedBindings, actualBindings);
	}
	
	@Test
	public void testNotNull() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().get(0).setAggregatedAssociation(null);
		test.getAggregatedCollection().remove(1);

		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// allAttributesPersistents[0]
		expectedBindings.add(Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 0));

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
			
		}.visit(d, test, c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}
	
	@Test
	public void testInverses() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 2);
		// Load inverses
		test = p.save(test);
		p.evictAllCached();
		test = p.retrieve(d, test.getBizId(), false);

		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// inverse[0]
		String expectedBinding = Binder.createIndexedBinding(AllAttributesPersistent.inverseAggregatedAssociationPropertyName, 0);
		expectedBindings.add(expectedBinding);
		expectedBinding = Binder.createCompoundBinding(expectedBinding, AllAttributesPersistent.aggregatedCollectionPropertyName);
		// inverse[0].aggregatedCollection[0]
		expectedBindings.add(Binder.createIndexedBinding(expectedBinding, 0));
		// inverse[0].aggregatedCollection[1]
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
			
		}.visit(d, test.getAggregatedAssociation(), c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}
	
	@Test
	public void testScalar() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 2);
		test.setAggregatedAssociation(test);
		test.getAggregatedCollection().set(0, test);
		
		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// aggregatedCollection[1]
		expectedBindings.add(Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 1));

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
			
		}.visit(d, test, c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}

	@Test
	public void testVector() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 2);
		test.getAggregatedCollection().set(0, test.getAggregatedAssociation());
		
		Set<String> expectedBindings = new TreeSet<>();
		expectedBindings.add("");
		// aggregatedAssociation
		expectedBindings.add(AllAttributesPersistent.aggregatedAssociationPropertyName);
		// aggregatedCollection[0]
		expectedBindings.add(Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 0));
		// aggregatedCollection[1]
		expectedBindings.add(Binder.createIndexedBinding(AllAttributesPersistent.aggregatedCollectionPropertyName, 1));

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
			
		}.visit(d, test, c);

		Assert.assertEquals(expectedBindings, actualBindings);
	}
}
