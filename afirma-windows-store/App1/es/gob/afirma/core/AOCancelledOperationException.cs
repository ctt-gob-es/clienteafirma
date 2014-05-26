using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace es.gob.afirma.core
{
    class AOCancelledOperationException : Exception
    {
        public AOCancelledOperationException() : base("Operación cancelada por el usuario")
        {
        }
    }
}
