package org.skyve.impl.util;

@Deprecated
public class BeanShellUtil
{
/*
	private static final String IMPORTS = "import java.text.*; \n" + 
											"import org.skyve.impl.util.*; \n" +
											"import org.skyve.impl.util.beanshell.*; \n" +
											"import org.skyve.impl.domain.*; \n" +											
											"import org.skyve.impl.metadata.security.User; \n" + 
											"import org.apache.commons.beanutils.DynaBean; \n";
*/
	private BeanShellUtil()
	{
		// no implementation
	}
/*
	public static <T> T getScriptedInterface(String code)
	throws MetaDataException
	{
		T result = null;
		
		try
		{
			Interpreter i = new Interpreter();
			i.eval(IMPORTS);
			i.setStrictJava(true);
			result = (T) i.eval(code);
		}
		catch (EvalError e)
		{
			throw new MetaDataException(e);
		}
		
		return result;		
	}
	
	public static Object evaluateExpression(Bean bean, User user, String code)
	throws MetaDataException
	{
		Object result = null;
		
		try
		{
			Interpreter i = new Interpreter();
			i.eval(IMPORTS);
			i.set("bean", bean);
			i.set("user", user);
			i.setStrictJava(true);

			result = i.eval(code);
		}
		catch (EvalError e)
		{
			throw new MetaDataException(e);
		}
		
		return result;
	}
	
	public static boolean evaluateCondition(Bean bean, User user, String condition)
	throws MetaDataException
	{
		boolean result = false;
		
		Object o = BeanShellUtil.evaluateExpression(bean, user, condition);
		if (o instanceof Boolean)
		{
			result = ((Boolean) o).booleanValue();
		}
		else
		{
			throw new MetaDataException("The expression " + condition + " yields " + o + 
											" which is not boolean.");
		}
		
		return result;
	}
*/
}
