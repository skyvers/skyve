package modules.testModule.domain;

public class RelatedDoc extends org.skyve.impl.domain.AbstractPersistentBean {
	private static final long serialVersionUID = 1L;

	@Override
	public String getBizKey() {
		return getBizId();
	}

	@Override
	public String getBizModule() {
		return "testModule";
	}

	@Override
	public String getBizDocument() {
		return "RelatedDoc";
	}
}
