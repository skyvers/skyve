package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.user.User;

import modules.kitchensink.domain.InlineGrid;
import modules.kitchensink.domain.KitchenSink;
import modules.kitchensink.domain.OrderedGrid;
import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

/**
 * Tests {@link WebUtil#addNewElement}, which both the PrimeFaces and SmartClient data grids add through.
 */
class WebUtilAddNewElementH2Test extends AbstractSkyveTest {
	@Test
	void addsANewElementWithItsDefaultValues() throws Exception {
		KitchenSink kitchenSink = KitchenSink.newInstance();

		Bean element = WebUtil.addNewElement(u, kitchenSink, KitchenSink.inlineGridPropertyName, kitchenSink, mockWebContext());

		InlineGrid added = assertInstanceOfInlineGrid(element);
		assertEquals(1, kitchenSink.getInlineGrid().size());
		assertSame(added, kitchenSink.getInlineGrid().get(0));
		assertNotNull(added.getBizId());
		// default value expressions run on the server
		assertEquals("#000000", added.getColour());
		assertNull(added.getBizOrdinal());
	}

	@Test
	void givesAnOrderedChildCollectionElementTheNextOrdinal() throws Exception {
		KitchenSink kitchenSink = KitchenSink.newInstance();
		kitchenSink.addOrderedGridElement(OrderedGrid.newInstance());
		kitchenSink.addOrderedGridElement(OrderedGrid.newInstance());

		Bean element = WebUtil.addNewElement(u, kitchenSink, KitchenSink.orderedGridPropertyName, kitchenSink, mockWebContext());

		assertEquals(3, kitchenSink.getOrderedGrid().size());
		assertSame(element, kitchenSink.getOrderedGrid().get(2));
		assertEquals(Integer.valueOf(3), kitchenSink.getOrderedGrid().get(2).getBizOrdinal());
	}

	@Test
	void addsANewElementToAnInverseMany() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();

		Bean element = WebUtil.addNewElement(u,
												bean,
												AllAttributesPersistent.inverseAggregatedAssociationPropertyName,
												bean,
												mockWebContext());

		assertNotNull(element);
		assertTrue(bean.getInverseAggregatedAssociation().contains(element), bean.getInverseAggregatedAssociation()::toString);
		assertSame(bean, ((AllAttributesPersistent) element).getAggregatedAssociation());
	}

	@Test
	void runsTheInterceptorsWithTheParentBean() throws Exception {
		KitchenSink kitchenSink = KitchenSink.newInstance();
		CustomerImpl customer = spy((CustomerImpl) c);
		User user = userOf(customer, true);

		Bean element = WebUtil.addNewElement(user, kitchenSink, KitchenSink.inlineGridPropertyName, kitchenSink, mockWebContext());

		verify(customer).interceptBeforePreExecute(eq(ImplicitActionName.Add), same(element), same(kitchenSink), any());
		verify(customer).interceptAfterPreExecute(eq(ImplicitActionName.Add), same(element), same(kitchenSink), any());
	}

	@Test
	void addsNothingWhenAnInterceptorVetoes() throws Exception {
		KitchenSink kitchenSink = KitchenSink.newInstance();
		CustomerImpl customer = spy((CustomerImpl) c);
		doReturn(Boolean.TRUE).when(customer).interceptBeforePreExecute(any(), any(), any(), any());
		User user = userOf(customer, true);

		Bean element = WebUtil.addNewElement(user, kitchenSink, KitchenSink.inlineGridPropertyName, kitchenSink, mockWebContext());

		assertNull(element);
		assertTrue(kitchenSink.getInlineGrid().isEmpty());
		verify(customer, never()).interceptAfterPreExecute(any(), any(), any(), any());
	}

	@Test
	void refusesAUserWhoCannotCreateTheElementDocumentOfAPersistentRelation() {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		User user = userOf((CustomerImpl) c, false);
		AbstractWebContext webContext = mockWebContext();

		assertThrows(SecurityException.class,
						() -> WebUtil.addNewElement(user, bean, AllAttributesPersistent.aggregatedCollectionPropertyName, bean, webContext));
		assertTrue(bean.getAggregatedCollection().isEmpty());
	}

	@Test
	void doesNotCheckCreatePrivilegeForATransientDocument() throws Exception {
		// KitchenSink and its grid documents are not persistent, so nothing is created in the database
		KitchenSink kitchenSink = KitchenSink.newInstance();
		User user = userOf((CustomerImpl) c, false);

		assertNotNull(WebUtil.addNewElement(user, kitchenSink, KitchenSink.inlineGridPropertyName, kitchenSink, mockWebContext()));
	}

	@Test
	void refusesABindingThatIsNotARelation() {
		KitchenSink kitchenSink = KitchenSink.newInstance();
		AbstractWebContext webContext = mockWebContext();

		IllegalStateException e = assertThrows(IllegalStateException.class,
												() -> WebUtil.addNewElement(u, kitchenSink, KitchenSink.textPropertyName, kitchenSink, webContext));
		assertEquals("text doesn't resolve to a relation.", e.getMessage());
	}

	private static InlineGrid assertInstanceOfInlineGrid(Bean element) {
		assertTrue(element instanceof InlineGrid, String.valueOf(element));
		return (InlineGrid) element;
	}

	/**
	 * Creates a user of the given customer that can, or cannot, create any document.
	 */
	private User userOf(CustomerImpl customer, boolean canCreate) {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getName()).thenReturn(u.getName());
		when(user.getId()).thenReturn(u.getId());
		when(user.getDataGroupId()).thenReturn(u.getDataGroupId());
		when(Boolean.valueOf(user.canCreateDocument(any()))).thenReturn(Boolean.valueOf(canCreate));
		return user;
	}
}
