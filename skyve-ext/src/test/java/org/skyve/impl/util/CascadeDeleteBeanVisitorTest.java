package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;

@SuppressWarnings({"static-method", "boxing"})
class CascadeDeleteBeanVisitorTest {
	@Test
	void acceptProcessesPersistedRootBean() throws Exception {
		RecordingVisitor visitor = new RecordingVisitor();
		Bean bean = bean(true);
		Document document = mock(Document.class);

		assertThat(invokeAccept(visitor, "root", document, null, null, bean), is(true));
		assertThat(visitor.processed, is(1));
	}

	@Test
	void acceptRejectsUnpersistedBean() throws Exception {
		RecordingVisitor visitor = new RecordingVisitor();

		assertThat(invokeAccept(visitor, "root", mock(Document.class), null, null, bean(false)), is(false));
		assertThat(visitor.processed, is(0));
	}

	@Test
	void acceptRejectsParentNonPersistentAndAggregatedReferences() throws Exception {
		RecordingVisitor visitor = new RecordingVisitor();
		Document document = mock(Document.class);
		Bean bean = bean(true);

		assertThat(invokeAccept(visitor, "contact" + ChildBean.CHILD_PARENT_NAME_SUFFIX, document, document, relation(false), bean), is(false));
		assertThat(invokeAccept(visitor, "contact", document, document, reference(AssociationType.aggregation, true), bean), is(false));
		assertThat(invokeAccept(visitor, "contact", document, document, reference(AssociationType.embedded, true), bean), is(false));
		assertThat(invokeAccept(visitor, "contact", document, document, reference(CollectionType.aggregation, true), bean), is(false));
		assertThat(visitor.processed, is(0));
	}

	@Test
	void acceptProcessesPersistedNonAggregatedReference() throws Exception {
		RecordingVisitor visitor = new RecordingVisitor();
		Document document = mock(Document.class);
		Bean bean = bean(true);

		assertThat(invokeAccept(visitor, "contact", document, document, reference(AssociationType.composition, true), bean), is(true));
		assertThat(visitor.processed, is(1));
	}

	private static boolean invokeAccept(CascadeDeleteBeanVisitor visitor,
											String binding,
											Document visitedDocument,
											Document owningDocument,
											Relation owningRelation,
											Bean visitedBean)
	throws Exception {
		Method method = CascadeDeleteBeanVisitor.class.getDeclaredMethod("accept",
				String.class,
				Document.class,
				Document.class,
				Relation.class,
				Bean.class);
		method.setAccessible(true);
		return ((Boolean) method.invoke(visitor, binding, visitedDocument, owningDocument, owningRelation, visitedBean)).booleanValue();
	}

	private static Bean bean(boolean persisted) {
		Bean bean = mock(Bean.class);
		when(bean.isPersisted()).thenReturn(Boolean.valueOf(persisted));
		return bean;
	}

	private static Relation relation(boolean persistent) {
		Relation relation = mock(Relation.class);
		when(relation.isPersistent()).thenReturn(Boolean.valueOf(persistent));
		return relation;
	}

	private static Reference reference(org.skyve.metadata.model.document.Reference.ReferenceType type, boolean persistent) {
		Reference reference = mock(Reference.class);
		when(reference.isPersistent()).thenReturn(Boolean.valueOf(persistent));
		when(reference.getType()).thenReturn(type);
		return reference;
	}

	private static final class RecordingVisitor extends CascadeDeleteBeanVisitor {
		private int processed;

		@Override
		public void preDeleteProcessing(Document documentToCascade, Bean beanToCascade) {
			processed++;
		}
	}
}
