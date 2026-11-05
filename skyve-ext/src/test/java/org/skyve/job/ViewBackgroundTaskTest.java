package org.skyve.job;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.skyve.domain.Bean;

@SuppressWarnings("static-method")
class ViewBackgroundTaskTest {
	private static class RecordingViewBackgroundTask extends ViewBackgroundTask<Bean> {
		private int executeCalls;
		private Bean executedBean;

		@Override
		public void execute(Bean bean) {
			executeCalls++;
			executedBean = bean;
		}
	}

	@Test
	void executeBeanCallbackRecordsInvocation() {
		RecordingViewBackgroundTask task = new RecordingViewBackgroundTask();
		Bean bean = mock(Bean.class);

		task.execute(bean);

		assertEquals(1, task.executeCalls);
		assertThat(task.executedBean, is(sameInstance(bean)));
	}

	@Test
	void getBeanReadsInternalBeanField() throws Exception {
		RecordingViewBackgroundTask task = new RecordingViewBackgroundTask();
		Bean bean = mock(Bean.class);
		Field field = ViewBackgroundTask.class.getDeclaredField("bean");
		field.setAccessible(true);
		field.set(task, bean);

		assertThat(task.getBean(), is(sameInstance(bean)));
	}

	@Test
	void quartzExecuteWithEmptyContextSwallowsSetupFailure() throws Exception {
		RecordingViewBackgroundTask task = new RecordingViewBackgroundTask();
		JobExecutionContext context = mock(JobExecutionContext.class);
		when(context.getMergedJobDataMap()).thenReturn(new JobDataMap());

		task.execute(context);

		assertEquals(0, task.executeCalls);
		assertNull(task.executedBean);
		assertNull(task.getBean());
	}
}
