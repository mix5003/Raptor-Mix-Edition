using System;
using System.Collections.Generic;
using System.Text;

namespace NClass.Core
{
    public interface RAPTORUpdater
    {
        object createClass(string name, NClass.Core.ClassType ct);
        void deleteClass(object theClass);
        void renameClass(object theClass, string name);
        object createMethod(object theClass, string name, Method method);
        bool makeAbstract(object theClass, object subchart);
        void deleteMethod(object theClass, object subchart);
        void renameMethod(object theClass, object subchart, string name);
        void changeParameters(object theClass,
            object subchart,
            int num_params, string[] param_names, bool[] param_is_input, bool[] param_is_output);
        void reorderMethods(object theClass, IEnumerable<Operation> operations);
        void resetAttributes(object theClass, IEnumerable<Field> fields);
    }
}
