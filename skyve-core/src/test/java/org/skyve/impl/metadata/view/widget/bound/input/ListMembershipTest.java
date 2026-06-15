package org.skyve.impl.metadata.view.widget.bound.input;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class ListMembershipTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultCandidatesHeading() {
		ListMembership lm = new ListMembership();
		assertThat(lm.getCandidatesHeading(), is("Candidates"));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultMembersHeading() {
		ListMembership lm = new ListMembership();
		assertThat(lm.getMembersHeading(), is("Members"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setCandidatesHeadingRoundtrip() {
		ListMembership lm = new ListMembership();
		lm.setCandidatesHeading("Available");
		assertThat(lm.getCandidatesHeading(), is("Available"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setMembersHeadingRoundtrip() {
		ListMembership lm = new ListMembership();
		lm.setMembersHeading("Selected");
		assertThat(lm.getMembersHeading(), is("Selected"));
	}

	@Test
	@SuppressWarnings("static-method")
	void blankCandidatesHeadingBecomesNull() {
		ListMembership lm = new ListMembership();
		lm.setCandidatesHeading("  ");
		assertNull(lm.getCandidatesHeading());
	}

	@Test
	@SuppressWarnings("static-method")
	void setPixelWidthRoundtrip() {
		ListMembership lm = new ListMembership();
		lm.setPixelWidth(Integer.valueOf(300));
		assertThat(lm.getPixelWidth(), is(Integer.valueOf(300)));
	}

	@Test
	@SuppressWarnings("static-method")
	void setMinPixelHeightRoundtrip() {
		ListMembership lm = new ListMembership();
		lm.setMinPixelHeight(Integer.valueOf(200));
		assertThat(lm.getMinPixelHeight(), is(Integer.valueOf(200)));
	}

	@Test
	@SuppressWarnings("static-method")
	void changedActionsInitiallyEmpty() {
		ListMembership lm = new ListMembership();
		assertTrue(lm.getChangedActions().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void propertiesInitiallyEmpty() {
		ListMembership lm = new ListMembership();
		assertTrue(lm.getProperties().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedCandidatesHeadingWithNullReturnsNull() {
		ListMembership lm = new ListMembership();
		lm.setCandidatesHeading(null);
		assertNull(lm.getLocalisedCandidatesHeading());
	}

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedMembersHeadingWithNullReturnsNull() {
		ListMembership lm = new ListMembership();
		lm.setMembersHeading(null);
		assertNull(lm.getLocalisedMembersHeading());
	}
}
