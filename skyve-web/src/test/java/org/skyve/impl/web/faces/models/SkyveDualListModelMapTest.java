package org.skyve.impl.web.faces.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.primefaces.model.DualListModel;
import org.skyve.domain.Bean;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import jakarta.faces.FacesException;

@SuppressWarnings("static-method")
class SkyveDualListModelMapTest {

	@Test
	void getWithNullViewCreatesEmptyDualListModel() {
		SkyveDualListModelMap map = new SkyveDualListModelMap(null);

		DualListModel<?> model = map.get("roles");

		assertNotNull(model);
		assertTrue(model.getSource().isEmpty());
		assertTrue(model.getTarget().isEmpty());
		assertSame(model, map.get("roles"));
	}

	@Test
	void getWithNullCurrentBeanCreatesEmptyDualListModel() {
		FacesView view = mock(FacesView.class);
		when(view.getCurrentBean()).thenReturn(null);
		SkyveDualListModelMap map = new SkyveDualListModelMap(view);

		DualListModel<?> model = map.get("memberships");

		assertTrue(model.getSource().isEmpty());
		assertTrue(model.getTarget().isEmpty());
		assertEquals(1, map.size());
	}

	@Test
	void getWithNullBeanCreatesEmptyDualListModel() {
		FacesView view = mock(FacesView.class);
		BeanMapAdapter adapter = new BeanMapAdapter(null, null);
		when(view.getCurrentBean()).thenReturn(adapter);
		SkyveDualListModelMap map = new SkyveDualListModelMap(view);

		DualListModel<?> model = map.get("tags");

		assertTrue(model.getSource().isEmpty());
		assertTrue(model.getTarget().isEmpty());
	}

	@Test
	void gatherReturnsImmediatelyWhenViewIsNull() {
		SkyveDualListModelMap map = new SkyveDualListModelMap(null);
		map.put("roles", new DualListModel<>());

		map.gather();

		assertEquals(1, map.size());
	}

	@Test
	void gatherReturnsImmediatelyWhenCurrentBeanIsNull() {
		FacesView view = mock(FacesView.class);
		when(view.getCurrentBean()).thenReturn(null);
		SkyveDualListModelMap map = new SkyveDualListModelMap(view);
		map.put("roles", new DualListModel<>());

		map.gather();

		assertEquals(1, map.size());
	}

	@Test
	void gatherReturnsImmediatelyWhenWrappedBeanIsNull() {
		FacesView view = mock(FacesView.class);
		BeanMapAdapter adapter = new BeanMapAdapter(null, null);
		when(view.getCurrentBean()).thenReturn(adapter);
		SkyveDualListModelMap map = new SkyveDualListModelMap(view);
		map.put("roles", new DualListModel<>());

		map.gather();

		assertEquals(1, map.size());
	}

	@Test
	void getUsesExistingEntryWithoutReScatter() {
		FacesView view = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, null);
		when(view.getCurrentBean()).thenReturn(adapter);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");

		SkyveDualListModelMap map = new SkyveDualListModelMap(view);
		DualListModel<DomainValue> existing = new DualListModel<>();
		map.put("roles", existing);

		DualListModel<?> result = map.get("roles");

		assertSame(existing, result);
	}

	@Test
	void getWrapsScatterFailuresWithBindingContext() {
		FacesView view = mock(FacesView.class);
		Bean bean = mock(Bean.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, null);
		when(view.getCurrentBean()).thenReturn(adapter);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");
		SkyveDualListModelMap map = new SkyveDualListModelMap(view);

		FacesException thrown = assertThrows(FacesException.class, () -> map.get("roles"));

		assertTrue(thrown.getMessage().contains("Could not scatter list membership roles"));
	}
}
