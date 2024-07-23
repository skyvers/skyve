package modules.test.AllAttributesDynamicPersistent;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.web.WebContext;

public class TestBizlet extends org.skyve.metadata.model.document.Bizlet<DynamicPersistentBean> {
	@Override
	public DynamicPersistentBean newInstance(DynamicPersistentBean bean) throws Exception {
		return super.newInstance(bean);
	}

	@Override
	public void validate(DynamicPersistentBean bean, ValidationException e) throws Exception {
		super.validate(bean, e);
	}

	@Override
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {
		return super.getConstantDomainValues(attributeName);
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		return super.getVariantDomainValues(attributeName);
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, DynamicPersistentBean bean) throws Exception {
		return super.getDynamicDomainValues(attributeName, bean);
	}

	@Override
	public List<String> complete(String attributeName, String value, DynamicPersistentBean bean) throws Exception {
		return super.complete(attributeName, value, bean);
	}

	@Override
	public DynamicPersistentBean resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
		return super.resolve(bizId, conversationBean, webContext);
	}

	@Override
	public void preSave(DynamicPersistentBean bean) throws Exception {
		super.preSave(bean);
	}

	@Override
	public void postSave(DynamicPersistentBean bean) throws Exception {
		super.postSave(bean);
	}

	@Override
	public void preDelete(DynamicPersistentBean bean) throws Exception {
		super.preDelete(bean);
	}

	@Override
	public void postLoad(DynamicPersistentBean bean) throws Exception {
		super.postLoad(bean);
	}

	@Override
	public DynamicPersistentBean preExecute(ImplicitActionName actionName,
												DynamicPersistentBean bean, Bean parentBean,
												WebContext webContext)
	throws Exception {
		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	@Override
	public void preRerender(String source, DynamicPersistentBean bean, WebContext webContext) throws Exception {
		super.preRerender(source, bean, webContext);
	}
}
