__kernel void somekernel(__global int *dst)
{
        int id = get_global_id(0);
        dst[id] = 0;
}