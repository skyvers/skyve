package org.skyve.util;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Inverse.InverseCardinality;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;

@SuppressWarnings("static-method")
class BeanVisitorTest {
	private interface SelfBean extends Bean {
		Bean getSelf();
	}

	private interface BadSelfBean extends Bean {
		Object getSelf();
	}

	private interface GraphBean extends Bean {
		Bean getChild();
		List<Bean> getChildren();
		Bean getParent();
	}

	private interface DualChildBean extends Bean {
		Bean getChildA();
		Bean getChildB();
	}

	private interface InverseBean extends Bean {
		Bean getInverseRel();
	}

	private static class RecordingBeanVisitor extends BeanVisitor {
		private final List<String> bindings = new ArrayList<>();

		RecordingBeanVisitor(boolean acceptVisited) {
			this(false, false, acceptVisited);
		}

		RecordingBeanVisitor(boolean visitInverses, boolean vectorCyclicDetection, boolean acceptVisited) {
			super(visitInverses, vectorCyclicDetection, acceptVisited);
		}

		@Override
		protected boolean accept(String binding,
								Document document,
								Document owningDocument,
								Relation owningRelation,
								Bean bean) {
			bindings.add(binding);
			return true;
		}
	}

	private static class RecordingNullableVisitor extends NullableBeanVisitor {
		private final List<String> bindings = new ArrayList<>();

		RecordingNullableVisitor() {
			super(false, false, false);
		}

		@Override
		protected boolean acceptNulls(String binding,
									Document document,
									Document owningDocument,
									Relation owningRelation,
									Bean bean) {
			bindings.add(binding);
			return true;
		}
	}

	private static Document selfReferencingDocument(Customer customer, Module module, Association relation) {
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("Contact");
		@SuppressWarnings({ "rawtypes", "unchecked" })
		List<Attribute> attributes = (List) List.of(relation);
		doReturn(attributes).when(document).getAllAttributes(customer);
		when(document.getParentDocument(customer)).thenReturn(null);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		return document;
	}

	private static Document graphDocument(Customer customer,
										Module module,
										String name,
										List<Attribute> attributes,
										Document parent) {
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn(name);
		doReturn(attributes).when(document).getAllAttributes(customer);
		when(document.getParentDocument(customer)).thenReturn(parent);
		when(module.getDocument(customer, name)).thenReturn(document);
		return document;
	}

	private static void mockBeanIdentity(Bean bean) {
		when(bean.getBizId()).thenReturn("biz-1");
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
	}

	@Test
	void beanVisitorSkipsAlreadyVisitedInstancesByDefault() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Association relation = mock(Association.class);
		SelfBean bean = mock(SelfBean.class);
		Document document = selfReferencingDocument(customer, module, relation);
		when(customer.getModule("admin")).thenReturn(module);
		when(relation.getName()).thenReturn("self");
		when(relation.getDocumentName()).thenReturn("Contact");
		when(bean.getSelf()).thenReturn(bean);
		mockBeanIdentity(bean);

		RecordingBeanVisitor visitor = new RecordingBeanVisitor(false);
		visitor.visit(document, bean, customer);
		assertThat(visitor.bindings, contains(""));
	}

	@Test
	void beanVisitorCanAcceptVisitedInstancesWhenConfigured() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Association relation = mock(Association.class);
		SelfBean bean = mock(SelfBean.class);
		Document document = selfReferencingDocument(customer, module, relation);
		when(customer.getModule("admin")).thenReturn(module);
		when(relation.getName()).thenReturn("self");
		when(relation.getDocumentName()).thenReturn("Contact");
		when(bean.getSelf()).thenReturn(bean);
		mockBeanIdentity(bean);

		RecordingBeanVisitor visitor = new RecordingBeanVisitor(true);
		visitor.visit(document, bean, customer);
		assertThat(visitor.bindings, contains("", "self"));
	}

	@Test
	void nullableVisitorTraversesNullRelations() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Association relation = mock(Association.class);
		Document document = selfReferencingDocument(customer, module, relation);
		when(customer.getModule("admin")).thenReturn(module);
		when(relation.getName()).thenReturn("self");
		when(relation.getDocumentName()).thenReturn("Contact");

		RecordingNullableVisitor visitor = new RecordingNullableVisitor();
		visitor.visit(document, null, customer);
		assertThat(visitor.bindings, contains("", "self"));
	}

	@Test
	void classCastFromBinderIsWrappedAsDomainException() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Association relation = mock(Association.class);
		BadSelfBean bean = mock(BadSelfBean.class);
		Document document = selfReferencingDocument(customer, module, relation);
		when(customer.getModule("admin")).thenReturn(module);
		when(relation.getName()).thenReturn("self");
		when(relation.getDocumentName()).thenReturn("Contact");
		when(bean.getSelf()).thenReturn("notABean");
		mockBeanIdentity(bean);

		RecordingBeanVisitor visitor = new RecordingBeanVisitor(false);
		DomainException ex = assertThrows(DomainException.class, () -> visitor.visit(document, bean, customer));
		assertThat(ex.getMessage(), containsString("Possible bean accessor clash"));
	}

	@Test
	void beanVisitorTraversesAssociationCollectionAndParent() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("admin")).thenReturn(module);

		Association childRelation = mock(Association.class);
		when(childRelation.getName()).thenReturn("child");
		when(childRelation.getDocumentName()).thenReturn("ChildDoc");

		Relation childrenRelation = mock(Relation.class);
		when(childrenRelation.getName()).thenReturn("children");
		when(childrenRelation.getDocumentName()).thenReturn("ChildDoc");

		Document parentDoc = graphDocument(customer, module, "ParentDoc", Collections.emptyList(), null);
		graphDocument(customer, module, "ChildDoc", Collections.emptyList(), null);
		Document rootDoc = graphDocument(customer,
									module,
									"RootDoc",
									List.of(childRelation, childrenRelation),
									parentDoc);

		GraphBean root = mock(GraphBean.class);
		Bean child = mock(Bean.class);
		Bean collectionChild = mock(Bean.class);
		Bean parent = mock(Bean.class);
		List<Bean> children = new ArrayList<>();
		children.add(collectionChild);
		children.add(null);

		mockBeanIdentity(root);
		when(root.getBizId()).thenReturn("root");
		when(root.getBizDocument()).thenReturn("RootDoc");
		when(root.getChild()).thenReturn(child);
		when(root.getChildren()).thenReturn(children);
		when(root.getParent()).thenReturn(parent);

		when(child.getBizId()).thenReturn("child");
		when(child.getBizModule()).thenReturn("admin");
		when(child.getBizDocument()).thenReturn("ChildDoc");

		when(collectionChild.getBizId()).thenReturn("collectionChild");
		when(collectionChild.getBizModule()).thenReturn("admin");
		when(collectionChild.getBizDocument()).thenReturn("ChildDoc");

		when(parent.getBizId()).thenReturn("parent");
		when(parent.getBizModule()).thenReturn("admin");
		when(parent.getBizDocument()).thenReturn("ParentDoc");

		RecordingBeanVisitor visitor = new RecordingBeanVisitor(false);
		visitor.visit(rootDoc, root, customer);

		assertThat(visitor.bindings, contains("", "child", "children[0]", "parent"));
	}

	@Test
	void nullableVisitorVisitsEmptyCollectionBindingWhenVisitNullsEnabled() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("admin")).thenReturn(module);

		Relation childrenRelation = mock(Relation.class);
		when(childrenRelation.getName()).thenReturn("children");
		when(childrenRelation.getDocumentName()).thenReturn("ChildDoc");

		graphDocument(customer, module, "ChildDoc", Collections.emptyList(), null);
		Document rootDoc = graphDocument(customer,
									module,
									"RootDoc",
									List.of(childrenRelation),
									null);

		GraphBean root = mock(GraphBean.class);
		when(root.getBizId()).thenReturn("root");
		when(root.getBizModule()).thenReturn("admin");
		when(root.getBizDocument()).thenReturn("RootDoc");
		when(root.getChildren()).thenReturn(Collections.emptyList());

		RecordingNullableVisitor visitor = new RecordingNullableVisitor();
		visitor.visit(rootDoc, root, customer);

		assertThat(visitor.bindings, contains("", "children"));
	}

	@Test
	void beanVisitorSkipsInverseRelationsWhenVisitInversesDisabled() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("admin")).thenReturn(module);

		Inverse inverse = mock(Inverse.class);
		when(inverse.getName()).thenReturn("inverseRel");
		when(inverse.getDocumentName()).thenReturn("ChildDoc");
		when(inverse.getCardinality()).thenReturn(InverseCardinality.one);

		graphDocument(customer, module, "ChildDoc", Collections.emptyList(), null);
		Document rootDoc = graphDocument(customer,
									module,
									"RootDoc",
									List.of(inverse),
									null);

		InverseBean root = mock(InverseBean.class);
		when(root.getBizId()).thenReturn("root");
		when(root.getBizModule()).thenReturn("admin");
		when(root.getBizDocument()).thenReturn("RootDoc");
		when(root.getInverseRel()).thenReturn(mock(Bean.class));

		RecordingBeanVisitor visitor = new RecordingBeanVisitor(false, false, false);
		visitor.visit(rootDoc, root, customer);

		assertThat(visitor.bindings, contains(""));
	}

	@Test
	void vectorCyclicDetectionAllowsSameBeanViaDifferentRelations() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("admin")).thenReturn(module);

		Association relA = mock(Association.class);
		when(relA.getName()).thenReturn("childA");
		when(relA.getDocumentName()).thenReturn("ChildDoc");

		Association relB = mock(Association.class);
		when(relB.getName()).thenReturn("childB");
		when(relB.getDocumentName()).thenReturn("ChildDoc");

		graphDocument(customer, module, "ChildDoc", Collections.emptyList(), null);
		Document rootDoc = graphDocument(customer,
									module,
									"RootDoc",
									List.of(relA, relB),
									null);

		DualChildBean root = mock(DualChildBean.class);
		Bean sharedChild = mock(Bean.class);
		when(root.getBizId()).thenReturn("root");
		when(root.getBizModule()).thenReturn("admin");
		when(root.getBizDocument()).thenReturn("RootDoc");
		when(root.getChildA()).thenReturn(sharedChild);
		when(root.getChildB()).thenReturn(sharedChild);

		when(sharedChild.getBizId()).thenReturn("shared");
		when(sharedChild.getBizModule()).thenReturn("admin");
		when(sharedChild.getBizDocument()).thenReturn("ChildDoc");

		RecordingBeanVisitor nonVector = new RecordingBeanVisitor(false, false, false);
		nonVector.visit(rootDoc, root, customer);
		assertThat(nonVector.bindings, contains("", "childA"));

		RecordingBeanVisitor vector = new RecordingBeanVisitor(false, true, false);
		vector.visit(rootDoc, root, customer);
		assertThat(vector.bindings, contains("", "childA", "childB"));
	}

	@Test
	void beanVisitorAcceptNullsMethodAlwaysReturnsFalse() throws Exception {
		BeanVisitor visitor = new BeanVisitor(false, false) {
			@Override
			protected boolean accept(String binding,
									Document document,
									Document owningDocument,
									Relation owningRelation,
									Bean bean) {
				return true;
			}
		};

		boolean result = visitor.acceptNulls("", mock(Document.class), null, null, null);
		assertFalse(result);
	}

	@Test
	void nullableBeanVisitorAcceptDelegatesToAcceptNulls() throws Exception {
		NullableBeanVisitor visitor = new NullableBeanVisitor(false, false) {
			@Override
			protected boolean acceptNulls(String binding,
										Document document,
										Document owningDocument,
										Relation owningRelation,
										Bean bean) {
				return true;
			}
		};

		boolean result = visitor.accept("", mock(Document.class), null, null, mock(Bean.class));
		assertTrue(result);
	}
}
