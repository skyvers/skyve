package modules.whosin;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.skyve.domain.messages.DomainException;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.admin.Contact.ContactExtension;
import modules.whosin.domain.Office;
import modules.whosin.domain.Staff;
import modules.whosin.domain.WhosinDocument;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class LoadDemonstrationDataJobTest extends AbstractH2Test {
	@Test
	void cancelReturnsNull() {
		assertNull(new LoadDemonstrationDataJob().cancel());
	}

	@Test
	void executeAfterCancelLogsCaughtSetupFailure() throws Exception {
		LoadDemonstrationDataJob job = new LoadDemonstrationDataJob();
		job.cancel();

		job.execute();

		assertEquals(0, job.getPercentComplete());
		assertThat(job.getLog().get(0), containsString("Started Loading Demonstration Data"));
		assertThat(job.getLog().get(1), containsString("Encountered an error during the job run"));
	}

	// AbstractBean.equals() is final and compares bizIds, so even these mocks are matched with same()

	/**
	 * A failed delete is rolled back before the next one, or the next one's commit would roll back with it,
	 * and the staff after it are retrieved again, since the rollback detached them.
	 * (The test customer has no whosin module, so this is tested with mocks.)
	 */
	@Test
	void clearPreviousDataRollsBackAFailedStaffDeleteAndRetrievesTheStaffAfterIt() {
		Staff first = staff("first");
		Staff failing = staff("failing");
		Staff after = staff("after");
		Staff retrieved = staff("after");
		ContactExtension retrievedContact = retrieved.getContact();
		Persistence persistence = mock(Persistence.class);
		DocumentQuery staffQuery = query(List.of(first, failing, after));
		DocumentQuery officeQuery = query(List.of());
		when(persistence.newDocumentQuery(WhosinDocument.STAFF)).thenReturn(staffQuery);
		when(persistence.newDocumentQuery(WhosinDocument.OFFICE)).thenReturn(officeQuery);
		doThrow(new DomainException("blocked")).when(persistence).delete(same(failing));
		doReturn(retrieved).when(persistence).retrieve(WhosinDocument.STAFF, "after");

		clearPreviousData(persistence);

		InOrder order = inOrder(persistence);
		order.verify(persistence).delete(same(first));
		order.verify(persistence).commit(false);
		order.verify(persistence).begin();
		order.verify(persistence).delete(same(failing));
		order.verify(persistence).rollback();
		order.verify(persistence).begin();
		order.verify(persistence).delete(same(retrieved));
		order.verify(persistence).delete(retrievedContact);
		order.verify(persistence).commit(false);
		verify(persistence, never()).delete(same(after));
	}

	@Test
	void clearPreviousDataRollsBackAFailedOfficeDeleteAndRetrievesTheOfficesAfterIt() {
		Office failing = office("failing");
		Office after = office("after");
		Office retrieved = office("after");
		Persistence persistence = mock(Persistence.class);
		DocumentQuery staffQuery = query(List.of());
		DocumentQuery officeQuery = query(List.of(failing, after));
		when(persistence.newDocumentQuery(WhosinDocument.STAFF)).thenReturn(staffQuery);
		when(persistence.newDocumentQuery(WhosinDocument.OFFICE)).thenReturn(officeQuery);
		doThrow(new DomainException("blocked")).when(persistence).delete(same(failing));
		doReturn(retrieved).when(persistence).retrieve(WhosinDocument.OFFICE, "after");

		clearPreviousData(persistence);

		InOrder order = inOrder(persistence);
		order.verify(persistence).delete(same(failing));
		order.verify(persistence).rollback();
		order.verify(persistence).begin();
		order.verify(persistence).delete(same(retrieved));
		order.verify(persistence).commit(false);
		verify(persistence, never()).delete(same(after));
	}

	private static Staff staff(String bizId) {
		Staff result = mock(Staff.class);
		when(result.getBizId()).thenReturn(bizId);
		ContactExtension contact = mock(ContactExtension.class);
		when(result.getContact()).thenReturn(contact);
		return result;
	}

	private static Office office(String bizId) {
		Office result = mock(Office.class);
		when(result.getBizId()).thenReturn(bizId);
		return result;
	}

	private static DocumentQuery query(List<?> results) {
		DocumentQuery result = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		when(result.getFilter()).thenReturn(filter);
		doReturn(results).when(result).beanResults();
		return result;
	}

	private static void clearPreviousData(Persistence persistence) {
		new LoadDemonstrationDataJob().clearPreviousData(persistence);
	}
}
