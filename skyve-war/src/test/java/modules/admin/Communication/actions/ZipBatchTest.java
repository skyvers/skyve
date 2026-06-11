package modules.admin.Communication.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import modules.admin.Communication.CommunicationExtension;

@SuppressWarnings("static-method")
class ZipBatchTest {

	@Test
	void prepareSetRefreshBatchesToFalse() throws Exception {
		ZipBatch action = new ZipBatch();
		CommunicationExtension bean = new CommunicationExtension();
		bean.setRefreshBatches(Boolean.TRUE);
		action.prepare(bean, null);
		assertEquals(Boolean.FALSE, bean.getRefreshBatches());
	}
}
