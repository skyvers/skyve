package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.primefaces.component.column.Column;
import org.primefaces.component.datatable.DataTable;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;

class ListGridBuilderTest {

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	@BeforeAll
	static void setUpFacesContext() {
		FacesContext facesContext = mock(FacesContext.class);
		Application mockApplication = mock(Application.class);
		ExpressionFactory mockExpressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		when(facesContext.getApplication()).thenReturn(mockApplication);
		when(facesContext.getELContext()).thenReturn(elContext);
		when(mockApplication.getExpressionFactory()).thenReturn(mockExpressionFactory);
		FacesContextBridge.setCurrent(facesContext);
	}

	@AfterAll
	static void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	// ---- PaginatedListGridBuilder ----

	@Test
	@SuppressWarnings("static-method")
	void paginatedListGridBuilderNullComponentReturnsNull() {
		PaginatedListGridBuilder builder = new PaginatedListGridBuilder();
		UIComponent result = builder.listGrid(null, "mod", "doc", "model", "uxui", null, null, null, false);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void paginatedListGridBuilderWithDataTableSetsPaginator() {
		PaginatedListGridBuilder builder = new PaginatedListGridBuilder();
		DataTable dataTable = mock(DataTable.class);
		UIComponent result = builder.listGrid(dataTable, "mod", "doc", "model", "uxui", null, null, null, false);
		assertSame(dataTable, result);
		verify(dataTable).setPaginator(true);
		verify(dataTable).setRowsPerPageTemplate("25,50,75,100");
		verify(dataTable).setPaginatorAlwaysVisible(false);
	}

	// ---- UnsortableListGridBuilder ----

	@Test
	@SuppressWarnings("static-method")
	void unsortableListGridBuilderNullComponentReturnsNull() {
		UnsortableListGridBuilder builder = new UnsortableListGridBuilder();
		UIComponent result = builder.listGrid(null, "mod", "doc", "model", "uxui", null, null, null, false);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void unsortableListGridBuilderDisablesSortingOnColumns() {
		UnsortableListGridBuilder builder = new UnsortableListGridBuilder();
		DataTable dataTable = mock(DataTable.class);
		Column col1 = mock(Column.class);
		Column col2 = mock(Column.class);
		List<UIComponent> children = new ArrayList<>();
		children.add(col1);
		children.add(col2);
		when(dataTable.getChildren()).thenReturn(children);

		UIComponent result = builder.listGrid(dataTable, "mod", "doc", "model", "uxui", null, null, null, false);
		assertSame(dataTable, result);
		verify(col1).setSortable(false);
		verify(col2).setSortable(false);
	}

	// ---- UnfilterableListGridBuilder ----

	@Test
	@SuppressWarnings("static-method")
	void unfilterableListGridBuilderNullComponentReturnsNull() {
		UnfilterableListGridBuilder builder = new UnfilterableListGridBuilder();
		UIComponent result = builder.listGrid(null, "mod", "doc", "model", "uxui", null, null, null, false);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void unfilterableListGridBuilderDisablesFilteringOnColumns() {
		UnfilterableListGridBuilder builder = new UnfilterableListGridBuilder();
		DataTable dataTable = mock(DataTable.class);
		Column col = mock(Column.class);
		List<UIComponent> children = new ArrayList<>();
		children.add(col);
		when(dataTable.getChildren()).thenReturn(children);

		UIComponent result = builder.listGrid(dataTable, "mod", "doc", "model", "uxui", null, null, null, false);
		assertSame(dataTable, result);
		verify(col).setFilterable(false);
	}

	// ---- StickyHeaderListBuilder ----

	@Test
	@SuppressWarnings("static-method")
	void stickyHeaderListBuilderNullComponentReturnsNull() {
		StickyHeaderListBuilder builder = new StickyHeaderListBuilder();
		UIComponent result = builder.listGrid(null, "mod", "doc", "model", "uxui", null, null, null, false);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void stickyHeaderListBuilderWithDataTableSetsStickyHeader() {
		StickyHeaderListBuilder builder = new StickyHeaderListBuilder();
		DataTable dataTable = mock(DataTable.class);

		UIComponent result = builder.listGrid(dataTable, "mod", "doc", "model", "uxui", null, null, null, false);
		assertSame(dataTable, result);
		verify(dataTable).setStickyHeader(true);
		verify(dataTable).setStickyTopAt(".layout-topbar,#header");
	}

	@Test
	@SuppressWarnings("static-method")
	void stickyHeaderListBuilderIgnoresNonDataTableComponent() {
		StickyHeaderListBuilder builder = new StickyHeaderListBuilder();
		UIComponent component = mock(UIComponent.class);

		UIComponent result = builder.listGrid(component, "mod", "doc", "model", "uxui", null, null, null, false);
		assertSame(component, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void stickyHeaderListRepeaterNullComponentReturnsNull() {
		StickyHeaderListBuilder builder = new StickyHeaderListBuilder();
		UIComponent result = builder.listRepeater(null, "doc", "model", "uxui", null, null, null, true, true);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void stickyHeaderListRepeaterWithDataTableSetsStickyHeader() {
		StickyHeaderListBuilder builder = new StickyHeaderListBuilder();
		DataTable dataTable = mock(DataTable.class);

		UIComponent result = builder.listRepeater(dataTable, "doc", "model", "uxui", null, null, null, true, true);
		assertSame(dataTable, result);
		verify(dataTable).setStickyHeader(true);
		verify(dataTable).setStickyTopAt(".layout-topbar,#header");
	}
}
