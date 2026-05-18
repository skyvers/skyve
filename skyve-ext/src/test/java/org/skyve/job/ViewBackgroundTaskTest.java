package org.skyve.job;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.Test;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.skyve.domain.Bean;

@SuppressWarnings("static-method")
public class ViewBackgroundTaskTest {
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
	public void executeBeanCallbackRecordsInvocation() throws Exception {
		RecordingViewBackgroundTask task = new RecordingViewBackgroundTask();
		Bean bean = mock(Bean.class);

		task.execute(bean);

		assertEquals(1, task.executeCalls);
		assertThat(task.executedBean, is(sameInstance(bean)));
	}

	@Test
	public void getBeanReadsInternalBeanField() throws Exception {
		RecordingViewBackgroundTask task = new RecordingViewBackgroundTask();
		Bean bean = mock(Bean.class);
		Field field = ViewBackgroundTask.class.getDeclaredField("bean");
		field.setAccessible(true);
		field.set(task, bean);

		assertThat(task.getBean(), is(sameInstance(bean)));
	}

	@Test
	public void quartzExecuteWithEmptyContextSwallowsSetupFailure() throws Exception {
		RecordingViewBackgroundTask task = new RecordingViewBackgroundTask();
		JobExecutionContext context = mock(JobExecutionContext.class);
		when(context.getMergedJobDataMap()).thenReturn(new JobDataMap());

		task.execute(context);

		assertEquals(0, task.executeCalls);
		assertNull(task.executedBean);
		assertNull(task.getBean());
	}
}
