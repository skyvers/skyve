package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.ExportedReference;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.SQL;

@SuppressWarnings("static-method")
class ExportedReferenceVisitorTest {
	@AfterEach
	void teardown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void dereferencerPutDefaultReturnsSameInstanceForModuleDocumentAndDocumentOverload() {
		Document document = document("admin", "Communication", null);
		ExportedReferenceVisitor.Dereferencer dereferencer = new ExportedReferenceVisitor.Dereferencer();

		assertSame(dereferencer, dereferencer.putDefault("admin", "Communication", "fallback-id"));
		assertSame(dereferencer, dereferencer.putDefault(document, "other-id"));
	}

	@Test
	void visitBeanRecordsReferencesParentsAndDerivedDocuments() throws Exception {
		CustomerImpl customer = mock(CustomerImpl.class);
		Document target = document("admin", "Target", persistent("TARGET"));
		Document referenceDocument = document("admin", "Reference", persistent("REFERENCE"));
		Document parentDocument = document("admin", "Parent", persistent("PARENT"));
		Document derivedDocument = document("admin", "Derived", persistent("DERIVED"));
		Module module = mock(Module.class);
		ExportedReference ref = new ExportedReference();
		ref.setModuleName("admin");
		ref.setDocumentName("Reference");
		ref.setReferenceFieldName("target");
		ref.setType(AssociationType.aggregation);
		when(customer.getExportedReferences(target)).thenReturn(List.of(ref));
		when(customer.getExportedReferences(derivedDocument)).thenReturn(null);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Reference")).thenReturn(referenceDocument);
		when(module.getDocument(customer, "Derived")).thenReturn(derivedDocument);
		when(customer.getBaseDocument(target)).thenReturn(null);
		when(customer.getBaseDocument(derivedDocument)).thenReturn(null);
		when(customer.getDerivedDocuments(target)).thenReturn(List.of("admin.Derived"));
		when(customer.getDerivedDocuments(derivedDocument)).thenReturn(List.of());
		when(target.getParentDocument(customer)).thenReturn(parentDocument);
		when(derivedDocument.getParentDocument(customer)).thenReturn(null);
		RecordingVisitor visitor = new RecordingVisitor();

		invokeVisitBean(visitor, customer, target, "target-id");

		assertThat(visitor.references, is(List.of("admin.Target:target-id->admin.Reference")));
		assertThat(visitor.parents, is(List.of("admin.Target:target-id->admin.Parent")));
	}

	@Test
	void visitBeanSkipsAlreadyVisitedBizId() throws Exception {
		CustomerImpl customer = mock(CustomerImpl.class);
		Document target = document("admin", "Target", persistent("TARGET"));
		RecordingVisitor visitor = new RecordingVisitor();
		Method method = visitBeanMethod();
		Set<String> documentsVisited = new TreeSet<>();
		Set<String> bizIdsVisited = new TreeSet<>();
		bizIdsVisited.add("target-id");

		method.invoke(visitor, customer, target, "target-id", documentsVisited, bizIdsVisited);

		assertTrue(visitor.references.isEmpty());
		assertTrue(visitor.parents.isEmpty());
	}

	@Test
	void dereferencerUpdatesRequiredCollectionReferenceToDefaultOwner() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = fluentSQL();
		when(persistence.newSQL("update REFERENCE_target set owner_id = :owner_id where element_id = :bizId")).thenReturn(sql);
		Document target = document("admin", "Target", persistent("TARGET"));
		Document referenceDocument = document("admin", "Reference", persistent("REFERENCE"));
		ExportedReference ref = exportedReference(CollectionType.aggregation, true);
		ExportedReferenceVisitor.Dereferencer dereferencer = new ExportedReferenceVisitor.Dereferencer()
				.putDefault("admin", "Reference", "replacement-owner");

		invokeAcceptReference(dereferencer, target, "target-id", ref, referenceDocument);

		org.mockito.Mockito.verify(sql).putParameter(Bean.DOCUMENT_ID, "target-id", false);
		org.mockito.Mockito.verify(sql).putParameter(PersistentBean.OWNER_COLUMN_NAME, "replacement-owner", false);
		org.mockito.Mockito.verify(sql).execute();
	}

	@Test
	void dereferencerDeletesOptionalCollectionReference() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = fluentSQL();
		when(persistence.newSQL("delete from REFERENCE_target where element_id = :bizId")).thenReturn(sql);
		Document target = document("admin", "Target", persistent("TARGET"));
		Document referenceDocument = document("admin", "Reference", persistent("REFERENCE"));
		ExportedReference ref = exportedReference(CollectionType.aggregation, false);

		invokeAcceptReference(new ExportedReferenceVisitor.Dereferencer(), target, "target-id", ref, referenceDocument);

		org.mockito.Mockito.verify(sql).putParameter(Bean.DOCUMENT_ID, "target-id", false);
		org.mockito.Mockito.verify(sql).execute();
	}

	@Test
	void dereferencerUpdatesScalarReferenceToNullWhenOptional() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = fluentSQL();
		when(persistence.newSQL("update REFERENCE set target_id = :newBizId where target_id = :bizId")).thenReturn(sql);
		Document target = document("admin", "Target", persistent("TARGET"));
		Document referenceDocument = document("admin", "Reference", persistent("REFERENCE"));
		ExportedReference ref = exportedReference(AssociationType.aggregation, false);

		invokeAcceptReference(new ExportedReferenceVisitor.Dereferencer(), target, "target-id", ref, referenceDocument);

		org.mockito.Mockito.verify(sql).putParameter(Bean.DOCUMENT_ID, "target-id", false);
		org.mockito.Mockito.verify(sql).putParameter("newBizId", null, false);
		org.mockito.Mockito.verify(sql).execute();
	}

	@Test
	void dereferencerUpdatesHierarchicalParentToDefault() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = fluentSQL();
		when(persistence.newSQL("update NODE set bizParentId = :newBizId where bizParentId = :bizId")).thenReturn(sql);
		Document node = document("admin", "Node", persistent("NODE"));
		ExportedReferenceVisitor.Dereferencer dereferencer = new ExportedReferenceVisitor.Dereferencer()
				.putDefault("admin", "Node", "replacement-parent");

		invokeAcceptParent(dereferencer, node, "target-id", node);

		org.mockito.Mockito.verify(sql).putParameter(Bean.DOCUMENT_ID, "target-id", false);
		org.mockito.Mockito.verify(sql).putParameter("newBizId", "replacement-parent", false);
		org.mockito.Mockito.verify(sql).execute();
	}

	private static void invokeVisitBean(ExportedReferenceVisitor visitor, CustomerImpl customer, Document document, String bizId) throws Exception {
		visitBeanMethod().invoke(visitor, customer, document, bizId, new TreeSet<>(), new TreeSet<>());
	}

	private static Method visitBeanMethod() throws Exception {
		Method method = ExportedReferenceVisitor.class.getDeclaredMethod("visitBean",
				CustomerImpl.class,
				Document.class,
				String.class,
				Set.class,
				Set.class);
		method.setAccessible(true);
		return method;
	}

	private static void invokeAcceptReference(ExportedReferenceVisitor.Dereferencer dereferencer,
												Document document,
												String bizId,
												ExportedReference ref,
												Document referenceDocument)
	throws Exception {
		Method method = ExportedReferenceVisitor.Dereferencer.class.getDeclaredMethod("acceptReference",
				Document.class,
				String.class,
				ExportedReference.class,
				Document.class);
		method.setAccessible(true);
		method.invoke(dereferencer, document, bizId, ref, referenceDocument);
	}

	private static void invokeAcceptParent(ExportedReferenceVisitor.Dereferencer dereferencer,
											Document document,
											String bizId,
											Document parentDocument)
	throws Exception {
		Method method = ExportedReferenceVisitor.Dereferencer.class.getDeclaredMethod("acceptParent",
				Document.class,
				String.class,
				Document.class);
		method.setAccessible(true);
		method.invoke(dereferencer, document, bizId, parentDocument);
	}

	private static ExportedReference exportedReference(org.skyve.metadata.model.document.Reference.ReferenceType type, boolean required) {
		ExportedReference ref = new ExportedReference();
		ref.setReferenceFieldName("target");
		ref.setType(type);
		ref.setRequired(required);
		return ref;
	}

	private static SQL fluentSQL() {
		SQL sql = mock(SQL.class);
		when(sql.putParameter(anyString(), any(), eq(false))).thenReturn(sql);
		return sql;
	}

	private static AbstractPersistence bindMockPersistence() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		java.lang.reflect.Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
		return persistence;
	}

	private static void unbindPersistenceFromThread() throws Exception {
		java.lang.reflect.Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}

	@SuppressWarnings("boxing")
	private static Document document(String moduleName, String name, Persistent persistent) {
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn(moduleName);
		when(document.getName()).thenReturn(name);
		when(document.getPersistent()).thenReturn(persistent);
		when(document.isPersistable()).thenReturn(persistent != null);
		return document;
	}

	private static Persistent persistent(String identifier) {
		Persistent persistent = new Persistent();
		persistent.setName(identifier);
		return persistent;
	}

	private static final class RecordingVisitor extends ExportedReferenceVisitor {
		private final List<String> references = new ArrayList<>();
		private final List<String> parents = new ArrayList<>();

		@Override
		protected void acceptReference(Document document, String bizId, ExportedReference exportedReference, Document referenceDocument) {
			references.add(document.getOwningModuleName() + "." + document.getName() + ":" + bizId + "->" +
					referenceDocument.getOwningModuleName() + "." + referenceDocument.getName());
		}

		@Override
		protected void acceptParent(Document document, String bizId, Document parentDocument) {
			parents.add(document.getOwningModuleName() + "." + document.getName() + ":" + bizId + "->" +
					parentDocument.getOwningModuleName() + "." + parentDocument.getName());
		}
	}
}
